#include "cid.h"

#include <dirent.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define CIDS_DIR "cids"

struct mismatch {
    char *name;
    char *expected;
};

static int compare_strings(const void *a, const void *b) {
    const char *lhs = *(const char *const *)a;
    const char *rhs = *(const char *const *)b;
    return strcmp(lhs, rhs);
}

static char *duplicate_string(const char *value) {
    const size_t length = strlen(value) + 1;
    char *copy = malloc(length);
    if (!copy) {
        return NULL;
    }
    memcpy(copy, value, length);
    return copy;
}

static char *join_path(const char *left, const char *right) {
    const size_t length = strlen(left) + strlen(right) + 2;
    char *path = malloc(length);
    if (!path) {
        return NULL;
    }
    snprintf(path, length, "%s/%s", left, right);
    return path;
}

static bool read_file(const char *path, unsigned char **data, size_t *length) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        return false;
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        fclose(file);
        return false;
    }

    long size = ftell(file);
    if (size < 0) {
        fclose(file);
        return false;
    }

    if (fseek(file, 0, SEEK_SET) != 0) {
        fclose(file);
        return false;
    }

    const size_t allocation = size > 0 ? (size_t)size : 1;
    unsigned char *buffer = malloc(allocation);
    if (!buffer) {
        fclose(file);
        return false;
    }

    const size_t read = size > 0 ? fread(buffer, 1, (size_t)size, file) : 0;
    fclose(file);

    if (read != (size_t)size) {
        free(buffer);
        return false;
    }

    *data = buffer;
    *length = (size_t)size;
    return true;
}

static bool is_regular_file(const char *path) {
    struct stat info;
    return stat(path, &info) == 0 && S_ISREG(info.st_mode);
}

int main(void) {
    DIR *directory = opendir(CIDS_DIR);
    if (!directory) {
        fprintf(stderr, "Missing cids directory at %s\n", CIDS_DIR);
        return 1;
    }

    char **files = NULL;
    size_t file_count = 0;

    struct dirent *entry;
    while ((entry = readdir(directory)) != NULL) {
        if (entry->d_name[0] == '.') {
            continue;
        }
        char *path = join_path(CIDS_DIR, entry->d_name);
        if (!path) {
            closedir(directory);
            for (size_t i = 0; i < file_count; ++i) {
                free(files[i]);
            }
            free(files);
            return 1;
        }
        if (!is_regular_file(path)) {
            free(path);
            continue;
        }
        free(path);

        char **temp = realloc(files, (file_count + 1) * sizeof(char *));
        if (!temp) {
            closedir(directory);
            free(files);
            return 1;
        }
        files = temp;
        files[file_count] = duplicate_string(entry->d_name);
        if (!files[file_count]) {
            closedir(directory);
            for (size_t i = 0; i < file_count; ++i) {
                free(files[i]);
            }
            free(files);
            return 1;
        }
        ++file_count;
    }
    closedir(directory);

    qsort(files, file_count, sizeof(char *), compare_strings);

    struct mismatch *mismatches = NULL;
    size_t mismatch_count = 0;

    for (size_t i = 0; i < file_count; ++i) {
        const char *filename = files[i];

        char *path = join_path(CIDS_DIR, filename);
        if (!path) {
            fprintf(stderr, "Failed to build path for %s\n", filename);
            continue;
        }

        unsigned char *content = NULL;
        size_t length = 0;
        if (!read_file(path, &content, &length)) {
            fprintf(stderr, "Failed to read %s\n", path);
            free(path);
            continue;
        }
        free(path);

        char *expected = compute_cid(content, length);
        free(content);
        if (!expected) {
            fprintf(stderr, "Failed to compute CID for %s\n", filename);
            continue;
        }

        if (strcmp(filename, expected) != 0) {
            struct mismatch *temp = realloc(mismatches, (mismatch_count + 1) * sizeof(struct mismatch));
            if (!temp) {
                free(expected);
                continue;
            }
            mismatches = temp;
            mismatches[mismatch_count].name = duplicate_string(filename);
            mismatches[mismatch_count].expected = expected;
            ++mismatch_count;
        } else {
            free(expected);
        }
    }

    for (size_t i = 0; i < file_count; ++i) {
        free(files[i]);
    }
    free(files);

    if (mismatch_count > 0) {
        printf("Found CID mismatches:\n");
        for (size_t i = 0; i < mismatch_count; ++i) {
            printf("- %s should be %s\n", mismatches[i].name, mismatches[i].expected);
            free(mismatches[i].name);
            free(mismatches[i].expected);
        }
        free(mismatches);
        return 1;
    }

    free(mismatches);
    printf("All %zu CID files match their contents.\n", file_count);
    return 0;
}
