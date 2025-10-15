<!-- global.qmd  
<!-- Defines variables, functions used repeatedly across many chapters of -->
<!-- the book. Include this file at the beginning of every chapter file using -->
<!-- [ blank line ] -->
<!-- {{< include global.qmd >}} -->
<!-- [ blank line ] -->
<!-- See https://quarto.org/docs/authoring/includes.html -->





::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Error : package or namespace load failed for 'rjags':
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/rjags/libs/rjags.so':
  dlopen(/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <CAF5E1DC-317A-34FE-988A-FB6F7C73D89E> /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libjags.4.dylib' (no such file)
```


:::
:::








# Front matter {.unnumbered}

This is the front matter, in an `index.qmd` file *required* by Quarto because it assumes a website is being created.
