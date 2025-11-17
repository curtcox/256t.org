cmake_minimum_required(VERSION 3.20)

set(BASE_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
set(CIDS_DIR "${BASE_DIR}/cids")

set(PYTHON_CID_SCRIPT [=[
from pathlib import Path
import base64, hashlib, sys
path = Path(sys.argv[1])
data = path.read_bytes()
encode = lambda b: base64.urlsafe_b64encode(b).decode().rstrip('=')
prefix = encode(len(data).to_bytes(6, 'big'))
if len(data) <= 64:
    suffix = encode(data)
else:
    suffix = encode(hashlib.sha512(data).digest())
print(prefix + suffix)
]=])
string(REPLACE ";" "\\;" PYTHON_CID_SCRIPT_ESCAPED "${PYTHON_CID_SCRIPT}")

set(mismatches "")
set(count 0)

file(GLOB cid_files "${CIDS_DIR}/*")
list(SORT cid_files)

foreach(path IN LISTS cid_files)
  if(IS_DIRECTORY "${path}")
    continue()
  endif()
  math(EXPR count "${count} + 1")
  get_filename_component(filename "${path}" NAME)

  execute_process(
    COMMAND python -c "${PYTHON_CID_SCRIPT_ESCAPED}" "${path}"
    OUTPUT_VARIABLE expected
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  if(NOT "${filename}" STREQUAL "${expected}")
    list(APPEND mismatches "${filename}|${expected}")
  endif()
endforeach()

if(mismatches)
  message("Found CID mismatches:")
  foreach(entry IN LISTS mismatches)
    string(REPLACE "|" ";" parts "${entry}")
    list(GET parts 0 actual)
    list(GET parts 1 expected)
    message(" - ${actual} should be ${expected}")
  endforeach()
  message(FATAL_ERROR "CID mismatches detected")
else()
  message(STATUS "All ${count} CID files match their contents.")
endif()
