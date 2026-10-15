// Translations for the helper pages, helper messages shown by the public
// resolver, and the extension. English text is the key: locales/<code>.json
// maps it to the translation, with {name} placeholders for variable parts.
// Anything without an entry stays English.

export const languages = ['zh-Hans', 'pt-BR'];

const identity = Object.assign((text, values) => format(text, values), { language: 'en', message: text => text });

function format(text, values = {}) {
  return text.replace(/\{(\w+)\}/g, (whole, name) => (name in values ? String(values[name]) : whole));
}

function escape(text) {
  return text.replace(/[.*+?^$()|[\]\\]/g, '\\$&');
}

// Pick a supported language from preferences such as navigator.languages.
export function negotiate(preferred = []) {
  for (const value of preferred) {
    const tag = String(value).toLowerCase();
    if (tag === 'zh' || tag === 'zh-cn' || tag === 'zh-sg' || tag.startsWith('zh-hans')) return 'zh-Hans';
    if (tag === 'pt' || tag.startsWith('pt-')) return 'pt-BR';
    if (tag === 'en' || tag.startsWith('en-')) return 'en';
  }
  return 'en';
}

export async function translator(language, base = new URL('./locales/', import.meta.url)) {
  if (!languages.includes(language)) return identity;
  let catalog;
  try {
    const response = await fetch(new URL(`${language}.json`, base));
    if (!response.ok) return identity;
    catalog = await response.json();
  } catch { return identity; }

  // Keys with placeholders also translate text produced elsewhere, such as
  // "Provider requires path" from the helper.
  const templates = Object.keys(catalog).filter(key => /\{\w+\}/.test(key)).map(key => {
    const names = [];
    const pattern = escape(key).replace(/\\?\{(\w+)\\?\}/g, (whole, name) => { names.push(name); return '(.+?)'; });
    return { key, names, pattern: new RegExp(`^${pattern}$`, 's') };
  });

  const t = (text, values) => format(catalog[text] ?? text, values);
  const line = text => {
    if (text in catalog) return catalog[text];
    for (const { key, names, pattern } of templates) {
      const match = pattern.exec(text);
      if (match) return format(catalog[key], Object.fromEntries(names.map((name, index) => [name, match[index + 1]])));
    }
    // "provider: reason" and "HTTP 404: reason" translate their reason.
    const labelled = /^([^:\n]{1,80}): (.+)$/s.exec(text);
    if (labelled) {
      const reason = line(labelled[2]);
      if (reason !== labelled[2]) return `${labelled[1]}: ${reason}`;
    }
    return text;
  };
  t.language = language;
  // Translate a message produced by other code, line by line.
  t.message = text => String(text).split('\n').map(line).join('\n');
  return t;
}
