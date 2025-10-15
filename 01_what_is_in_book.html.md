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








# What's in this book (Read this first!) {#sec-overview}

<!-- ::: {.callout-note appearance="minimal"} -->
<!-- Oh honey I'm searching for love that is true,\ -->
<!-- But driving through fog is so dang hard to do.\ -->
<!-- Please paint me a line on the road to your heart,\ -->
<!-- I'll rev up my pick up and get a clean start.^[This chapter provides a road map to the book, which hopes to have you fall in love with Bayesian analysis even if you previously had unhappy relationships with statistics. The poem plays with those ideas.] -->
<!-- ::: -->

This is a reference: @knuth84.

## Real people can read this book






::: {.cell}

```{.r .cell-code}
table_df <- data.frame(
  property = c("Essential measure:", "Describe any distribution:", "Non-linear transformation invariant:", "Typical application:", "MCMC stability:"),
  mean_sd = c("Squared deviation", "Worst", "No", "Prior", "Best"),
  median_eti = c("Cumulative probability", "Middling", "Yes", "Posterior", "Middling"),
  mode_hdi = c("Probability density", "Best", "Only for discrete distributions", "Prior and posterior", "Worst"))

pdf_table_df <- data.frame(
  property = c("Essential measure:", "Describe any distribution:", "{Non-linear transformation \\\\ invariant:}", "Typical application:", "MCMC stability:"),
  mean_sd = c("{Squared \\\\ deviation}", "Worst", "No", "Prior", "Best"),
  median_eti = c("{Cumulative \\\\ probability}", "Middling", "Yes", "Posterior", "Middling"),
  mode_hdi = c("{Probability \\\\ density}", "Best", "{Only for discrete \\\\ distributions}", "Prior and posterior", "Worst"))

pdf_table_df |> 
  tt(width = c(2, 1.1, 1.1, 1.1)) |> 
  format_tt(i = "colnames", escape = TRUE) |>  # To handle the underscores and ampersands
  style_tt(j = 1, align = "l") |> 
  style_tt(j = 2:4, align = "c") |> 
  setNames(c("Property", "Mean & SD", "Median & ETI", "Mode & HDI"))
```

::: {.cell-output-display}


```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_kesyqq55qjehgh93e6rb = TinyTable.createTableFunctions("tinytable_kesyqq55qjehgh93e6rb");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 } ], css_id: 'tinytable_css_uvck9thnmhcf1jx8d02m',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 } ], css_id: 'tinytable_css_rvqv529uozv77bkt4wuo',}, 
          { positions: [ { i: '5', j: 2 }, { i: '5', j: 3 }, { i: '5', j: 4 } ], css_id: 'tinytable_css_lw5ci9vkxqhi7xag5056',}, 
          { positions: [ { i: '5', j: 1 } ], css_id: 'tinytable_css_w845g0ktqylz6aqfucnv',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 } ], css_id: 'tinytable_css_qtlxxamx834fdry5e2qy',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_y328vjeplqfwgzrxqhx0',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_kesyqq55qjehgh93e6rb.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_uvck9thnmhcf1jx8d02m, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_uvck9thnmhcf1jx8d02m { text-align: center; width: 21%; }
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_rvqv529uozv77bkt4wuo, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_rvqv529uozv77bkt4wuo { text-align: left; width: 37%; }
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_lw5ci9vkxqhi7xag5056, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_lw5ci9vkxqhi7xag5056 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center; width: 21%; }
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_w845g0ktqylz6aqfucnv, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_w845g0ktqylz6aqfucnv {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; width: 37%; }
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_qtlxxamx834fdry5e2qy, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_qtlxxamx834fdry5e2qy {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center; width: 21%; }
    #tinytable_kesyqq55qjehgh93e6rb td.tinytable_css_y328vjeplqfwgzrxqhx0, #tinytable_kesyqq55qjehgh93e6rb th.tinytable_css_y328vjeplqfwgzrxqhx0 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; width: 37%; }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_kesyqq55qjehgh93e6rb" style="table-layout: fixed; width: 100%; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Property</th>
                <th scope="col" data-row="0" data-col="2">Mean &amp; SD</th>
                <th scope="col" data-row="0" data-col="3">Median &amp; ETI</th>
                <th scope="col" data-row="0" data-col="4">Mode &amp; HDI</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Essential measure:</td>
                  <td data-row="1" data-col="2">{Squared \\ deviation}</td>
                  <td data-row="1" data-col="3">{Cumulative \\ probability}</td>
                  <td data-row="1" data-col="4">{Probability \\ density}</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Describe any distribution:</td>
                  <td data-row="2" data-col="2">Worst</td>
                  <td data-row="2" data-col="3">Middling</td>
                  <td data-row="2" data-col="4">Best</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">{Non-linear transformation \\ invariant:}</td>
                  <td data-row="3" data-col="2">No</td>
                  <td data-row="3" data-col="3">Yes</td>
                  <td data-row="3" data-col="4">{Only for discrete \\ distributions}</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">Typical application:</td>
                  <td data-row="4" data-col="2">Prior</td>
                  <td data-row="4" data-col="3">Posterior</td>
                  <td data-row="4" data-col="4">Prior and posterior</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">MCMC stability:</td>
                  <td data-row="5" data-col="2">Best</td>
                  <td data-row="5" data-col="3">Middling</td>
                  <td data-row="5" data-col="4">Worst</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```


:::
:::
