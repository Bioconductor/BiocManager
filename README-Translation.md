This is a work in progress.

Translation contributors:

- Robert Castelo (Spanish)
- Jiefei Wang (Chinese)
- Marcel Ramos (French)
- Kozo Nishida (Japanese)

## Orientation

The [Translating R Messages, R >= 3.0.0][translations30] document provides guidance.

BiocManager tries to follow the discipline that all messages / warnings / errors are formated by `BiocManager:::.msg()` which is based on `sprintf()`. This is perfect for translation! R relies on the GNU gettext system, which works well when the `sprintf(fmt = )` argument is the 'key' on which translations are based.


## The `.pot` file

The idea is that there's a file that describes the key / value pairs expected in a translation. It is in `po/R-BiocManager.pot` (this was done using the [potools][] package; potools is unfortunately not helpful beyond this initial step). message / warning / error strings where then manually collaged into this file, with a few minor adjustments in BiocManager R code in the process.

Take a look -- there are entries like

```
msgid "'repos' argument to 'install()' not allowed"
msgstr ""
```

The `msgid` is the string used in R code. Another entry is

```
msgid "%s %d packages to Bioconductor version '%s'? [y/n]: "
msgstr ""
```

and in the original BiocManager R code `sprintf()` the first `"%s"` would be substituted with `upgrade` or `downgrade`, the `%d` to the number of packages to be changed, and the second `%s` the string representation of the Bioconductor version. So something like `upgrade 5 packages to Bioconductor version '3.13'? [y/n]: `

[translations30]: https://developer.r-project.org/Translations30.html
[potools]: https://github.com/MichaelChirico/potools

## Changes to BiocManager

There are a few changes to the structure of individual messages.

The most important code change is in the `.msg()` function. The updated code uses `gettextf()` to intercept the `fmt` argument, and ask base R to translate it if possible. `gettextf()` looks for a translation of the `fmt=` argument, and performs `sprintf()`-style substitution on the result.

```
--- a/R/utilities.R
+++ b/R/utilities.R
@@ -39,11 +39,9 @@
     )
     ## Use this helper to format all error / warning / message text
 {
-    txt <- sprintf(fmt, ...)
+    txt <- gettextf(fmt, ..., domain = "R-BiocManager")
     if (wrap.) {
```

Calls to `sprintf()` elsewhere in the code were replaced with `gettextf()`, and the constant strings in `R/version.R` were wrapped in `gettext()` or `gettextf()` as appropriate.

## A translation -- `po/R-fr.po`

This message

```
msgid "'repos' argument to 'install()' not allowed"
```

is produced by a command such as

```
BiocManager::install(repos = "foo")
```

It's easy to produce, so a useful test case.

Following the [Translating R Messages, R >= 3.0.0][translations30] document, in this branch of the repository 

```
git checkout po-translation
```

run the command

```
msginit -i po/R-BiocManager.pot -o po/R-fr.po
```

`msginit` must be installed (part of the GNU gettext package). The output `R-fr.po` indicates that this is the French-language translation.

`R-fr.po` looks messier than `R-BiocManager.pot`, but only because the new lines / tabs / etc have been expanded. Find and translate each message

```
msgid "'repos' argument to 'install()' not allowed"
msgstr "l'argument 'repos' pour 'install()' est interdit"
```

Not all messages require translations.

## Installing the translation

Create a sub-folder in the `inst/` directory for the language translation. Note the path includes the language `fr`

```
mkdir -p inst/po/fr/LC_MESSAGES
```

Compile the translations into a format that is (a) installed with the package and (b) recognized by R (note the `.mo` extension).

```
msgfmt -c --statistics -o inst/po/fr/LC_MESSAGES/R-BiocManager.mo po/R-fr.po
```

## Testing

Install the package from the BiocManager directory

```
R CMD INSTALL .
```

And then try it out

```
$ LANGUAGE="fr" R -e "BiocManager::install(repos = 'foo')"
> BiocManager::install(repos = 'foo')
Erreur : l'argument 'repos' pour 'install()' est interdit
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

# Next steps / notes / questions

1. It would be great to have people experiment with this, with the understanding that it is highly likely that the translations will need to be re-done.

1. Extracting the messages has helped to clarify the sorts of information that is being communicated, and may lead to re-structuring of the messages and indeed the code.

1. The gettext system allows for plural forms, and we do not exploit these.

1. What is the role of the `msgstr` field in the `.pot` file? Perhaps there's a level of indirection that could be exploited, like `msgid "install-repos-not-allowed"` and then provide the format string?

1. The language field in the beginning of the po file does not have to be correct, so in the beginning of the file I can say

    ```
    "Language-Team: nonsense\n"
    "Language: nonsense\n"
    ```

   as long as the file name is correct, R has no problem finding the translation.
   
1. `tools::update_pkg_po()` in principle simplifies management of pot / po / mo files, but scans text for use of sprintf() / stop() / warning() / message(), and so is defeated by the indirection of `BiocManager:::.msg()`.
   
## Concerns / next steps

1. Understanding the translation machinery increases the knowledge required of the developer, so that new team members will have additional challenges when working with BiocManager.

1. Translations become out-of-date when the text of a message is changed or new messages are added. 

1. The manual process of curating the `.pot` file, and the need to validate translations (e.g., translations must have `%s`, ... in exactly the same order as the original) requires automation (R functions), further increasing the technical burden. Alternatively, the BiocManager code could be revised so that it works more transparently with `tools::update_pkg_po()`.

1. (from [hpages][]) Use of Bioconductor requires English-language skills that are much more demanding than those needed to understand error messages, so the translations provide little value.

1. (from [hpages][]) Translating BiocManager creates the false expectation that other Bioconductor packages will be translated.

1. (from [hpages][]) It's easy for users to enter messages in Google Translate instead.

1. (from [hpages][]) Having users report errors in non-English languages increases the amount of work required to understand problems reported on the support site, etc.

[hpages]: https://github.com/Bioconductor/BiocManager/pull/109#issuecomment-859101361
