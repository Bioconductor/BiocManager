I don't know what I'm doing! But...

## Orientation

I used the [Translating R Messages, R >= 3.0.0][translations30] document as a kind of guide. 

BiocManager tries to follow the discipline that all messages / warnings / errors are formated by `BiocManager:::.msg()` which is based on `sprintf()`. This is perfect for translation! R relies on the GNU gettext system, which works well when the `sprintf(fmt = )` argument is the 'key' on which translations are based.


## The `.pot` file

The idea is, I think, that there's a file that describes the key / value pairs expected in a translation. I've created this file in the directory `po/R-BiocManager.pot` this was done using the [potools][] package; potools is unfortunately not helpful beyond this initial step). I manually collated message / warning / error strings into this file, making a few minor adjustments in BiocManager R code in the process.

Take a look -- you can see entries like

```
msgid "'repos' argument to 'install()' not allowed"
msgstr ""
```

The `msgid` is the string I've used in R code. Another entry is

```
msgid "%s %d packages to Bioconductor version '%s'? [y/n]: "
msgstr ""
```

and in the original BiocManager R code `sprintf()` the first `"%s"` would be substituted with `upgrade` or `downgrade`, the `%d` to the number of packages to be changed, and the second `%s` the string representation of the Bioconductor version. So something like `upgrade 5 packages to Bioconductor version '3.13'? [y/n]: `

I'm not really sure what the `msgstr` field role here is -- perhaps there's a level of indirection that I could exploit, like `msgid "install-repos-not-allowed"` and then provide the format string.

[translations30]: https://developer.r-project.org/Translations30.html
[potools]: https://github.com/MichaelChirico/potools

## Changes to BiocManager

I made a few minor changes to the structure of messages, but the relevant change is in the `.msg()` function. I intercept the `fmt` argument, and ask base R to translate it if possible using `gettext()`. I'm expecting a `sprintf()`-style format string, but perhaps in a different language. I then use the translated string in `sprintf()` to create the message, and then format following whatever `.msg()` does.

```
--- a/R/utilities.R
+++ b/R/utilities.R
@@ -39,6 +39,7 @@
     )
     ## Use this helper to format all error / warning / message text
 {
+    fmt <- gettext(fmt) # translate, via 'po/R-BiocManager.pot'
     txt <- sprintf(fmt, ...)
     if (wrap.) {
         txt <- strwrap(
```

## A translation -- `po/R-en.po`

This message

```
msgid "'repos' argument to 'install()' not allowed"
```

is produced if I try to do something like

```
BiocManager::install(repos = "foo")
```

It's easy to produce, so a useful test case.

I don't really speak any languages other than my own idiosyncratic English, so I thought I'd provide a faux-english translation, aiming for `Ola! 'repos' argument to 'install()' not allowed`. 

Following the [Translating R Messages, R >= 3.0.0][translations30] document, in this branch of the repository and under the `po/` directory, I ran the command

```
msginit -i R-BiocManager.pot -o R-en.po
```

I need `msginit` installed (part of the GNU gettext package). And I've specified the output as `R-en.po` to indicate that this is the (faux) English-language translation.

`R-en.po` looks messier than `R-BiocManager.pot`, but only because the new lines / tabs / etc have been expanded. I found my message and provided a translation

```
msgid "'repos' argument to 'install()' not allowed"
msgstr "Ola! 'repos' argument to 'install()' not allowed"
```

Note that I did not have to provide a translation for all messages.

## Installing the translation

Still following [Translating R Messages, R >= 3.0.0][translations30], 

I'm still in the `po/` directory. I need to create a sub-folder in the `inst/` directory for my language translation. So I ran the command

```
mkdir -p ../inst/po/en/LC_MESSAGES
```

And then I compile my translations into a format that is (a) installed with the package and (b) recognized by R (note the `.mo` extension).

```
msgfmt -c --statistics -o ../inst/po/en/LC_MESSAGES/R-BiocManager.mo R-en.po
```

## Testing

OK, I'm not sure about `devtools::load_all()`, etc., so I'm going to stick with standard practices. I installed the package from the BiocManager directory

```
R CMD INSTALL .
```

And then I gave it a whirl

```
$ R -e "BiocManager::install(repos = 'foo')"
> BiocManager::install(repos = 'foo')
Error: Ola! 'repos' argument to 'install()' not allowed
Execution halted
```

Note the `Ola!` -- my translation has been successfully installed!

## What to target?

My understanding is that languages and dialects are treated hierarchically -- `es` provides a 'Spanish' translation, whereas `es_CA` is Catalan. I believe that if the user is in `es_CA`, but the only translation available is `es`, then they would recieve the `es` translation. Thus it seems like the greatest value comes from providing base translations, with dialects pursued on a second iteration.

# Next steps

It would be great to have people experiment with this, with the understanding that it is highly likely that the translations will need to be re-done.

Extracting the messages has helped to clarify the sorts of information that is being communicated, and may lead to re-structuring of the messages and indeed the code.

The gettext system allows for plural forms, and we do not exploit these.
