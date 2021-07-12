This is a work in progress.

Translation contributors:

- Robert Castelo (Spanish)
- Jiefei Wang (Chinese)
- Marcel Ramos (French)
- Kozo Nishida (Japanese)
- Marcel Ramos, Federico Marini, Dario Rhigelli, Davide Risso (Italian)

## Orientation

The [Translating R Messages, R >= 3.0.0][translations30] document provides guidance. We also use the [potools][] package.

During development, the package author writes code that makes calls to `stop()`, `warning()`, `message()`, `gettext()`, `gettextf()` or related functions. `gettextf()` is like `sprintf()`, with a `fmt` argument and `...` for substitutions. The arguments to these calls is collated into a translation template file with extension `.pot`, e.g., `po/R-BiocManager.pot`.

Language-specific `.po` files are derived from the `.pot` template, e.g., `po/R-fr.po` for the French translation. The translation team updates the `.po` file to contain translations from the original language to the langauge of the `.po` file.

Prior to a release, the `.pot` and `.po` files are compiled to a series of `.mo` files that are distributed and installed with the package, e.g., `inst/po/fr/LC_MESSAGES/R-BiocManager.mo`.

At runtime, the text arguments in calls to `stop()`, etc., are compared to the any available translation for the language under which R is running. If there is a translation, it is used in place of the original text argument. If there is no translation, the original string is used.

[translations30]: https://developer.r-project.org/Translations30.html
[potools]: https://github.com/MichaelChirico/potools

## Package preparation

It makes sense to structure the package so that communication with the user is clear, and translation proceeds smoothly. For BiocManager, this involved (1) code re-organization and (2) refactoring of message strings.

### Code reorganization

BiocManager originally had calls such as `stop(msg)`. This cannot be added to the `.pot` template file  automatically because the value of `fmt` cannot be determined until run-time. Some `msg` strings were formed using `paste`, e.g., `paste0("Bioconductor version ', bioc_version, "' requires R version '", R_version, "'.")`. This form requires two phrases to be translated separately, with the translation of each phrase taken out of the context provided by the entire string.

Code was therefore refactored to always use `gettext() / gettextf()` to formulate the `msg`, rather than relying on `stop()` etc., to automatically identify strings for translation

```
msg <- gettextf(
   "Bioconductor version '%s' requires R version '%s'.",
    bioc_version, R_version
)
stop(msg)
```

There is some value to consistent use of this approach -- a later maintainer may well identify the pattern and either follow it without thinking, or dig into understanding what `gettextf()` does.

### Refactoring message strings

Many message strings were condensed, improper sentences. An example was `"argument 'repos' to 'install()' not allowed"`. This brevity causes problems for the translator, and presumably for the user struggling to understand implied sentence structure. All message strings were revised to be complete sentences. The new message string for this example is `Using 'repos' as an argument to 'install()' is not allowed."`. This process meant that each message string had to be carefully reviewed for accuracy and clarity, improving the quality of message presented to the user.

Some messages were excessively clever, e.g., `%s %d packages?`, where the first substitution was either 'Upgrade' or 'Downgrade'. The translator would have to separately translate 'Upgrade' and 'Downgrade', and would have to understand the context in which these words, and 'packages', were being used. It is better to refactor the code and message strings to require two translations `Upgrade %d packages?`, `Downgrade %d packages?`.

## Translation

The command `potools::translate_package()`

1. Creates or updates the template `po/R-BiocManager.pot` file.

2. Updates any existing translation files (e.g., `po/R-fr.po`).

3. Compiles translations to `inst/po/...` for installation.

The workflow is that a 'release manager' runs `potools::translate_package()` to generate or update the `.pot` and `.po` files. The release manager then informs the translation team to review and update their `po/*po` files. At the end of the translation period, the release manager again runs `potools::translate_package()` to create the updated, compiled messages. Note that the translation team does not require any special knowledge of the package.

### New translations

Check out the package repository from git. Translations are currently on the `po-translations` branch.

```
git checkout po-translation
```

Following the [Translating R Messages, R >= 3.0.0][translations30] document, run the command

```
msginit -i po/R-BiocManager.pot -o po/R-fr.po
```

`msginit` must be installed (part of the GNU gettext package). The output `R-fr.po` indicates that this is the French-language translation.

The `po/R-fr.po` file starts with

```
msgid ""
msgstr ""
"Project-Id-Version: BiocManager 1.30.15.3\n"
"POT-Creation-Date: 2021-07-05 10:28\n"
"PO-Revision-Date: 2021-06-10 10:31-0400\n"
"Last-Translator: Marcel Ramos <marcel.ramosperez@roswellpark.org>\n"
"Language-Team: French\n"
"Language: fr\n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
"Plural-Forms: nplurals=2; plural=(n != 1);\n"

msgid "Library paths: '%s'."
msgstr ""
...
```

Update the `Last-Translator`, `Language-Team`, and `Language` fields.

Proceed to translate each `msgid` to a `msgstr`. The convention is to break long `msgid` and `msgstr` into 80-character lines.

```
msgid ""
"Some library paths are not writeable, so 'install()' is not able to update "
"all packages."
msgstr ""
"...
```

Not all messages require translations. Untranslated messages are presented to the user in the original language.

### Updating translations

Update the header lines as appropriate.

```
"PO-Revision-Date: 2021-06-10 10:31-0400\n"
"Last-Translator: Marcel Ramos <marcel.ramosperez@roswellpark.org>\n"
"Language-Team: French\n"
"Language: fr\n"
```

An updated translation might require new messages to be translated. New messages appear in the `.po` file _without_ `msgstr`, e.g.,

```
msgid "Library paths: '%s'."
msgstr ""
```

The translator updates the `.po` file to include the translation

```
msgid "Library paths: '%s'."
msgstr "Chemins d'installation: '%s'."
```

The source of previously translated messages may have been updated, but the original and new message are sufficiently similar that the gettext tools recognize a 'fuzzy' match. These are prefixed with a comment `#, fuzzy`, e.g., a change from `old packages: '%s'` to `Out-of-date packages: '%s'.` results in this entry in the `.po` file.

```
#, fuzzy
msgid "Out-of-date packages: '%s'."
msgstr "pacquet logiciels anciens: '%s'"
```

The translator updates the translation and removes the fuzzy comment

```
msgid "Out-of-date packages: '%s'."
msgstr "Pacquet logiciels anciens: '%s'."
```

Translation with an empty `msgstr`, or with a `#, fuzzy` comment, are treated as 'untranslated', and the user sees the original English-language version.

## Testing

A translator may wish to test / preview their translation. Update (but do not commit to git) the `.mo` file.

```
R -e "potools::translate_package()
```

Install the package from the BiocManager directory

```
R CMD INSTALL .
```

And then try it out

```
$ LANGUAGE="fr" R -e "BiocManager::install(repos = 'foo')"
> BiocManager::install(repos = 'foo')
Erreur : L'argument 'repos' pour 'install()' est interdit
Exécution arrêtée
```

The translation has been successfully installed!

There seems to be OS-specific variation in how languages are set. Within _R_, it might be necessary to try any or all of

```
Sys.setenv(LANGUAGE = "fr")
Sys.setlocale(locale = "fr_FR")
Sys.setlocale("LC_MESSAGES", "fr_FR")
```

Available locales are determined by the operating system. See `?Sys.setlocale` for some furhter hints.

## What to target?

Languages and varieties are treated hierarchically -- `es` provides a 'Spanish' translation, whereas `es_MX` is Mexican Spanish. If the user is in `es_MX`, but the only translation available is `es`, then they receive the `es` translation. Thus it seems like the greatest value comes from providing base translations, with varieties pursued on a second iteration.

## Next steps / notes / questions

1. The gettext system allows for plural forms, and we do not exploit these.

## Concerns / next steps

1. Understanding the translation machinery increases the knowledge required of the developer, so that new team members will have additional challenges when working with BiocManager. New knowledge includes use of the `gettext() / gettextf()` discipline, and `potools::translate_package()`.

1. Translations become out-of-date when the text of a message is changed or new messages are added. However, out-of-date translations fail gracefully, reverting to the original English-language message.

1. (from nturaga) Users struggling with error messages will turn to google to find the error string. Translated errors will not match English-language errors, reducing the number of google hits available to understand the problem. BiocManager includes a 'hash' with each message string, e.g., `[id:ff3e2a]`. The hash is independent of the translation, so (hopefully) google will match the hash even in the face of translation.

1. (from [hpages][]) Use of Bioconductor requires English-language skills that are much more demanding than those needed to understand error messages, so the translations provide little value.

1. (from [hpages][]) Translating BiocManager creates the false expectation that other Bioconductor packages will be translated.

1. (from [hpages][]) It's easy for users to enter messages in Google Translate instead.

1. (from [hpages][]) Having users report errors in non-English languages increases the amount of work required to understand problems reported on the support site, etc.

[hpages]: https://github.com/Bioconductor/BiocManager/pull/109#issuecomment-859101361
