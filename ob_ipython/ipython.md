<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Introduction</a></li>
<li><a href="#orgheadline28">2. Features (aka "What's that powerful about it")</a>
<ul>
<li><a href="#orgheadline3">2.1. Embed code blocks in any language</a></li>
<li><a href="#orgheadline4">2.2. Results can be exported to many formats, like latex (demo), html (this post)</a></li>
<li><a href="#orgheadline5">2.3. Programmable documents (aka "Literate programming")</a></li>
<li><a href="#orgheadline9">2.4. Built in excel alternative</a>
<ul>
<li><a href="#orgheadline7">2.4.1. Export to pandas</a></li>
<li><a href="#orgheadline8">2.4.2. Other exporters</a></li>
</ul>
</li>
<li><a href="#orgheadline11">2.5. Pass data between languages</a></li>
<li><a href="#orgheadline12">2.6. Outline view is powerful for organizing your work</a></li>
<li><a href="#orgheadline13">2.7. Navigate to code and between org files with ctags.</a></li>
<li><a href="#orgheadline27">2.8. Many more</a>
<ul>
<li><a href="#orgheadline14">2.8.1. Embed latex formulas</a></li>
<li><a href="#orgheadline15">2.8.2. Fast integration with source control</a></li>
<li><a href="#orgheadline16">2.8.3. Mobile client</a></li>
<li><a href="#orgheadline17">2.8.4. Spaced repetition framework (remember all those pesky maths formulas)</a></li>
<li><a href="#orgheadline18">2.8.5. Calendar</a></li>
<li><a href="#orgheadline19">2.8.6. Managing papers citations</a></li>
<li><a href="#orgheadline20">2.8.7. Tagging</a></li>
<li><a href="#orgheadline21">2.8.8. Links</a></li>
<li><a href="#orgheadline22">2.8.9. Agenda views</a></li>
<li><a href="#orgheadline23">2.8.10. Quickly add something to current org, without interruption to flow</a></li>
<li><a href="#orgheadline24">2.8.11. Do lectures</a></li>
<li><a href="#orgheadline25">2.8.12. Go on a diet</a></li>
<li><a href="#orgheadline26">2.8.13. Even more</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline33">3. Installation</a>
<ul>
<li><a href="#orgheadline29">3.1. Install Emacs</a></li>
<li><a href="#orgheadline30">3.2. Install python packages</a></li>
<li><a href="#orgheadline31">3.3. Install org mode and ob-ipython</a></li>
<li><a href="#orgheadline32">3.4. Elisp configuration</a></li>
</ul>
</li>
<li><a href="#orgheadline38">4. Troubleshooting if something doesn't work</a>
<ul>
<li><a href="#orgheadline34">4.1. Verify that restarting ipython doesn't help.</a></li>
<li><a href="#orgheadline35">4.2. Open "Python" buffer to see python errors</a></li>
<li><a href="#orgheadline36">4.3. Toggle elisp debug on error</a></li>
<li><a href="#orgheadline37">4.4. Check project issues</a></li>
</ul>
</li>
<li><a href="#orgheadline41">5. My workflow</a>
<ul>
<li><a href="#orgheadline2">5.1. Screenshot</a></li>
<li><a href="#orgheadline39">5.2. Default ipython configuration</a></li>
<li><a href="#orgheadline40">5.3. <span class="todo TODO">TODO</span> Configure yasnippet</a></li>
</ul>
</li>
<li><a href="#orgheadline6">6. Examples</a>
<ul>
<li><a href="#orgheadline42">6.1. Org table to pandas and plotting</a></li>
<li><a href="#orgheadline43">6.2. Org table -&gt; Pandas -&gt; Org table</a></li>
<li><a href="#orgheadline44">6.3. <span class="todo TODO">TODO</span> Shared code</a></li>
<li><a href="#orgheadline45">6.4. <span class="todo TODO">TODO</span> Use global constant</a></li>
<li><a href="#orgheadline46">6.5. <span class="todo TODO">TODO</span> Data frame sharing with org tables</a></li>
<li><a href="#orgheadline10">6.6. <span class="todo TODO">TODO</span> Pass data directly between languages</a></li>
<li><a href="#orgheadline47">6.7. <span class="todo TODO">TODO</span> Different language kernels</a></li>
<li><a href="#orgheadline48">6.8. Examples from other blog posts</a></li>
</ul>
</li>
<li><a href="#orgheadline53">7. Unresolved problems</a>
<ul>
<li><a href="#orgheadline49">7.1. <span class="todo TODO">TODO</span> <code>ob-ipython-inspect</code> in popup</a></li>
<li><a href="#orgheadline50">7.2. <span class="todo TODO">TODO</span> Configure the <code>org-edit-src-code</code> to use ipython completion.</a></li>
<li><a href="#orgheadline51">7.3. <span class="todo TODO">TODO</span> Capture results from ipython to src block.</a></li>
<li><a href="#orgheadline52">7.4. <span class="todo TODO">TODO</span> Figure out why SVG doesn't work</a></li>
</ul>
</li>
<li><a href="#orgheadline60">8. Further reading</a>
<ul>
<li><a href="#orgheadline54">8.1. Official org mode documentation</a></li>
<li><a href="#orgheadline55">8.2. Official documentation of ob-ipython</a></li>
<li><a href="#orgheadline56">8.3. Research paper: An Effective Git And Org-Mode Based Workflow For Reproducible Research</a></li>
<li><a href="#orgheadline57">8.4. Run a whole research department on CMU using org mode</a></li>
<li><a href="#orgheadline58">8.5. Org mode for managing your server configuration</a></li>
<li><a href="#orgheadline59">8.6. Manage your emacs configuration using org mode</a></li>
</ul>
</li>
</ul>
</div>
</div>


# Introduction<a id="orgheadline1"></a>

Emacs `org-mode` with `ob-ipython` is the most powerful data analysis environment I ever used.
I find it much more powerful than other tools I used, including jupyter and beaker web notebooks or just writing python in PyCharm.

Emacs org mode with ob-ipython is like jupyter or beaker notebook, but in Emacs instead of browser and with many more features.

Word "Emacs" may be scary. There are pre-packaged and pre-configured emacs distribution that have much smaller learning curve, my favorite being [Spacemacs](http://spacemacs.org/) (I am in progress of rebasing my config with it).
You can just use 1% of capabilities of Emacs (probably majority of Emacs users do not approach 10% of Emacs capabilities) and still benefit from it.

If you are going to bring up the common quote of "emacs is fine operating system, but it lacks decent text editor" -
Emacs now have decent text editor by using the vim emulation `evil-mode`. It's the best vim emulation in existence
and even many packages from vim are ported. Spacemacs is a nice emacs distribution that bundles evil mode.

I will try to introduce and describe org mode with ob-ipython it for users who never used Emacs before.

Since this blog post have been written in org mode, linear reading experience in exported format is less optimal experience than reading [the org mode file](https://github.com/kozikow/kozikow-blog/blob/master/ob_ipython/ipython.org) directly in org mode.

# Features (aka "What's that powerful about it")<a id="orgheadline28"></a>

## Embed code blocks in any language<a id="orgheadline3"></a>

You can embed [embeded source code](http://orgmode.org/manual/Working-With-Source-Code.html) long text and evaluate it with `C-c C-c`.
It supports textual results or results as charts.

What's more You can have separate org file and ipython console open side by side.
With ipython, reading python docstrings and code completion works well. See [my screenshot](#orgheadline2).

Since ob-ipython uses jupyter, you can get the same environment for anything that have jupyter kernel, including [matlab](https://github.com/calysto/matlab_kernel), [Scala](https://github.com/alexarchambault/jupyter-scala), [Spark](https://github.com/apache/incubator-toree) or [R](http://irkernel.github.io/) and [many more](https://github.com/ipython/ipython/wiki/IPython-kernels-for-other-languages).

## Results can be exported to many formats, like latex [(demo)](https://github.com/kozikow/kozikow-blog/blob/master/ob_ipython/ipython.pdf), html [(this post)](https://kozikow.wordpress.com/2016/05/22/very-powerful-data-analysis-environment-org-mode-with-ob-ipython/)<a id="orgheadline4"></a>

[This blog post is just an export of org mode file.](https://github.com/kozikow/kozikow-blog/blob/master/ob_ipython/ipython.org) All code examples have been written in org mode.

Exporting works to formats like latex (native and beamer), markdown, jira, odt (than can be imported to google docs), wiki formats and many more.

Syntax highlighting can be preserved for some exports, like html or latex.

You can just learn one way to edit documents and presentations than can be exported to majority of formats on earth.

## Programmable documents (aka ["Literate programming"](https://en.wikipedia.org/wiki/Literate_programming))<a id="orgheadline5"></a>

Emacs org mode with org babel is a full fledged [literate programming](https://en.wikipedia.org/wiki/Literate_programming) environment.
[Some people have published whole books or research papers as a large executable document in org.](http://kitchingroup.cheme.cmu.edu/blog/2014/08/08/What-we-are-using-org-mode-for/) There is an even [Research paper about it](http://dl.acm.org/citation.cfm?id=2723881).

[Python computations in science and engineering](https://github.com/jkitchin/pycse) book supports org mode and
it's far better book reading experience than anything I ever experienced before.
I can tweak and re-run code examples, link from my other notes, tag or bookmark interesting
sections, jump between sections and many more.

When writing some latex in college, I recall situations when I am half
way through writing latex document. I would came up with the idea of
some parameter tweak, and suddenly I have to re-generate all charts.

With org mode, the document is generated pragmatically. Not only
you can easily re-generate it, but readers of your paper can tweak
parameters or supply their own data set and re-generate the whole document.

Another example is training machine model. You can define your model parameters as [org constants](http://orgmode.org/manual/In_002dbuffer-settings.html).
You can tweak some model parameter and have separate org mode headings for "performance statistics",
"top miss-classified cross validation samples", etc. Added benefit is that you can commit all this to git.

As soon as you learn org mode all of it is easy and seamless.

## [Built in excel alternative](http://orgmode.org/manual/The-spreadsheet.html#The-spreadsheet)<a id="orgheadline9"></a>

Sometimes just "manually" editing the data is the most productive thing to do.
You can do it with org mode spreadsheet capabilities on org tables.

The added benefit is that formulas are written in lisp, that is cooler and more powerful language than Visual basic.
<http://orgmode.org/manual/Translator-functions.html>

### Export to pandas<a id="orgheadline7"></a>

My current Table->Pandas->Table workflow works, but is a bit clunky, but it can be improved.
[See examples section](#orgheadline6).

### Other exporters<a id="orgheadline8"></a>

You can export org tables to many formats by exporting it to pandas and then
using pandas exporter.
Nevertheless, org supports [sql](https://github.com/stuartsierra/org-mode/blob/master/contrib/lisp/orgtbl-sqlinsert.el), [csv, latex, html](http://orgmode.org/manual/Translator-functions.html) exporters.

## Pass data between languages<a id="orgheadline11"></a>

Similar functionality is offered by [beaker notebook.](http://beakernotebook.com/)

I found out that org mode as intermediate format for data sometimes works better for me.

Since intermediate format for a data frame is the org table, I can import data frame to org, edit it as spreadsheet and export it back.
See [Pass data directly between languages](#orgheadline10) in examples section.

## Outline view is powerful for organizing your work<a id="orgheadline12"></a>

Org mode outline view is very handy for organizing your work.
When working on some larger problem, I am only focusing on small subset of it.
Org mode lets me just expand sections that are currently relevant.

I also find adding embedding TODO items in the tree quite handy.
When I encounter some problem I mark a subtree as TODO, and I can
later inspect just subtree headlines with TODO items with them.
See [![img](/home/rkoziko/home_org/blog/ob_ipython/todo.png)](todo.png).

## Navigate to code and between org files with ctags.<a id="orgheadline13"></a>

You can link to your existing codebase [with org-ctags.](http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=lisp/org-ctags.el;hb=HEAD)
It seems possible to provide ide-like navigation between
code defined in org src buffers, but I didn't configure it yet.

## Many more<a id="orgheadline27"></a>

You don't have to use all features offered by org mode.

### Embed latex formulas<a id="orgheadline14"></a>

Also works in html export with [mathjax.](https://www.mathjax.org/)

### Fast integration with source control<a id="orgheadline15"></a>

I like to keep my notes in source control.
To avoid overheard of additional committing I use `magit-mode`.
Out of the box you can commit directly from Emacs with 6 keyboard strokes.
With a few lines of elisp you can auto generate commit messages or automatically commit based on some condition (e.g. save or file closed or `focus-out-hook`).

Everything in org is plain text, including results of eval of code blocks, so it will be treated well by the source control.

### [Mobile client](https://play.google.com/store/apps/details?id=com.orgzly&hl=en_GB)<a id="orgheadline16"></a>

### Spaced repetition framework (remember all those pesky maths formulas)<a id="orgheadline17"></a>

If you are like me, you forgot majority of maths formulas since college.
[Very good post about spaced repetition in general from gwern.](https://www.gwern.net/Spaced%2520repetition)

There are tools like anki or super memo, but as soon as you want advanced features like
latex support they either not support them, or do it in a very bad way.

[org-drill](http://orgmode.org/worg/org-contrib/org-drill.html) is a spaced repetition framework in drill, that allows you to use all of the org features for creating flash cards.
[Also take a look at this interesting blog post](http://www.giovannicarmantini.com/2015/07/putting-some-make-up-on-my-org-mode-flashcards).

### Calendar<a id="orgheadline18"></a>

### Managing papers citations<a id="orgheadline19"></a>

[boq](https://github.com/kyleam/bog), [org-ref,](https://github.com/jkitchin/org-ref) [helm-bibtex,](https://github.com/tmalsburg/helm-bibtex) [some blog post](http://www.mkbehr.com/posts/a-research-workflow-with-zotero-and-org-mode/)

### Tagging<a id="orgheadline20"></a>

### Links<a id="orgheadline21"></a>

### Agenda views<a id="orgheadline22"></a>

### [Quickly add something to current org, without interruption to flow](http://orgmode.org/manual/Capture.html#Capture)<a id="orgheadline23"></a>

### [Do lectures](https://www.youtube.com/watch?v=JZ8RK-R9O_g)<a id="orgheadline24"></a>

### [Go on a diet](http://emacsporn.tumblr.com/post/4982654361/dieting-theres-an-org-mode-extension-for-that)<a id="orgheadline25"></a>

### Even more<a id="orgheadline26"></a>

I only mentioned some of the features. More urls that you can take a look at:

    (browse-url-emacs "http://kitchingroup.cheme.cmu.edu/org/2014/08/08/What-we-are-using-org-mode-for.org")

-   <http://orgmode.org/>
-   <http://doc.norang.ca/org-mode.html>
-   <http://pages.sachachua.com/.emacs.d/Sacha.html>
-   <https://github.com/fniessen/emacs-leuven-theme>
-   <https://github.com/purcell/color-theme-sanityinc-tomorrow>
-   <http://sachachua.com/blog/2014/01/tips-learning-org-mode-emacs/>
-   [Comparision with knitr](http://minimallysufficient.github.io/2015/10/24/org-mode-as-an-alternative-to-knitr.html)

# Installation<a id="orgheadline33"></a>

## Install Emacs<a id="orgheadline29"></a>

Although I don't use it, I recommend [Spacemacs](http://spacemacs.org/), pre-configured emacs distribution, like "Ubuntu" of Emacs.

## Install python packages<a id="orgheadline30"></a>

If you don't run those, you may run into troubles.

    pip install --upgrade pip
    pip install --upgrade ipython
    pip install --upgrade pyzmq
    pip install --upgrade jupyter

## Install org mode and ob-ipython<a id="orgheadline31"></a>

## Elisp configuration<a id="orgheadline32"></a>

Add to your Emacs config:

    (require 'org)
    (require 'ob-ipython)
    
    ;; don't prompt me to confirm everytime I want to evaluate a block
    (setq org-confirm-babel-evaluate nil)
    
    ;;; display/update images in the buffer after I evaluate
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

# Troubleshooting if something doesn't work<a id="orgheadline38"></a>

## Verify that restarting ipython doesn't help.<a id="orgheadline34"></a>

    (ob-ipython-kill-kernel)

## Open "Python" buffer to see python errors<a id="orgheadline35"></a>

## Toggle elisp debug on error<a id="orgheadline36"></a>

    (toggle-debug-on-error)

## [Check project issues](https://github.com/gregsexton/ob-ipython/issues)<a id="orgheadline37"></a>

# My workflow<a id="orgheadline41"></a>

I settled on workflow of having two buffers opened side by side.
On one side I would have opened org file, on the other side I would the have ipython console.

I am experimenting with commands in the ipython console, and I copy back the permanent results I want to
remember or share with people into the org src block.

Both windows re-use the same ipython kernel (So they share variables). You may have multiple kernels running.
I have code completion and python docstrings in the ipython buffer.

## Screenshot<a id="orgheadline2"></a>

![img](ob-ipython.png)

## Default ipython configuration<a id="orgheadline39"></a>

If you want to run some code in each ipython block you can add it to `~/.ipython/profile_default/startup`.
Foe example, to avoid adding `%matplotlib inline` to each source code block:

    echo "%matplotlib inline" >> ~/.ipython/profile_default/startup/66-matplot.py

## TODO Configure yasnippet<a id="orgheadline40"></a>

ob-ipython docs suggest yasnippet for editing code.
So far I have been using custom elisp code, but a few things can be nicer about yasnippet.

    # -*- mode: snippet -*-
    # name: ipython block
    # key: py
    # --
    #+BEGIN_SRC ipython :session ${1::file ${2:$$(let ((temporary-file-directory "./")) (make-temp-file "py" nil ".png"))} }:exports ${3:both}
    $0
    #+END_SRC

# Examples<a id="orgheadline6"></a>

## Org table to pandas and plotting<a id="orgheadline42"></a>

<table id="orgtable1" border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">date</th>
<th scope="col" class="org-right">x</th>
<th scope="col" class="org-right">y</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-15 Wed&gt;</span></span></td>
<td class="org-right">1</td>
<td class="org-right">1</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-16 Thu&gt;</span></span></td>
<td class="org-right">2</td>
<td class="org-right">2</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-17 Fri&gt;</span></span></td>
<td class="org-right">4</td>
<td class="org-right">3</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-18 Sat&gt;</span></span></td>
<td class="org-right">8</td>
<td class="org-right">4</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-19 Sun&gt;</span></span></td>
<td class="org-right">16</td>
<td class="org-right">5</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-20 Mon&gt;</span></span></td>
<td class="org-right">32</td>
<td class="org-right">6</td>
</tr>
</tbody>
</table>

    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd
    %matplotlib inline
    
    df = pd.DataFrame(table[1:], columns=table[0])
    df.plot()

![img](plot.png)

## Org table -> Pandas -> Org table<a id="orgheadline43"></a>

You have to write small reusable snippet to print pandas to org format.
You can add it to your builtin ipython code snippets.
You also need to tell src block to interpret results directly with `:results output raw drawer :noweb yes`.

    def arr_to_org(arr):
      line = "|".join(str(item) for item in arr)
      return "|{}|".format(line)
    
    def df_to_org(df):
      return "\n".join([arr_to_org(df.columns)] +
      [arr_to_org(row) for row in df.values])
    
    
    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd
    %matplotlib inline
    
    df = pd.DataFrame(table[1:], columns=table[0])
    df.y = df.y.apply(lambda y: y*2)
    print df_to_org(df)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<tbody>
<tr>
<td class="org-left">date</td>
<td class="org-right">x</td>
<td class="org-right">y</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-15 Wed&gt;</span></span></td>
<td class="org-right">1</td>
<td class="org-right">2</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-16 Thu&gt;</span></span></td>
<td class="org-right">2</td>
<td class="org-right">4</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-17 Fri&gt;</span></span></td>
<td class="org-right">4</td>
<td class="org-right">6</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-18 Sat&gt;</span></span></td>
<td class="org-right">8</td>
<td class="org-right">8</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-19 Sun&gt;</span></span></td>
<td class="org-right">16</td>
<td class="org-right">10</td>
</tr>


<tr>
<td class="org-left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2016-06-20 Mon&gt;</span></span></td>
<td class="org-right">32</td>
<td class="org-right">12</td>
</tr>
</tbody>
</table>

Afterwards, you may assign result table to variable, edit it with org spreadsheet capabilities and use in other python script.

## TODO Shared code<a id="orgheadline44"></a>

<http://emacs.stackexchange.com/questions/2951/can-i-include-a-common-code-block-in-two-different-code-blocks-in-org-mode>

## TODO Use global constant<a id="orgheadline45"></a>

## TODO Data frame sharing with org tables<a id="orgheadline46"></a>

## TODO Pass data directly between languages<a id="orgheadline10"></a>

Create my example based on <http://minimallysufficient.github.io/2015/10/24/org-mode-as-an-alternative-to-knitr.html>

## TODO Different language kernels<a id="orgheadline47"></a>

This should work:

    #+BEGIN_SRC ipython :session :kernel clojure
      (+ 1 2)
    #+END_SRC
    
    #+RESULTS:
    : 3

## Examples from other blog posts<a id="orgheadline48"></a>

`C-c C-c` block to open org file directly in Emacs:

    (browse-url-emacs "https://raw.githubusercontent.com/dfeich/org-babel-examples/master/python/pythonbabel.org")

    (browse-url-emacs "https://raw.githubusercontent.com/dfeich/org-babel-examples/master/python/ipython-babel.org")

# Unresolved problems<a id="orgheadline53"></a>

Problems I did not resolve yet:

## TODO `ob-ipython-inspect` in popup<a id="orgheadline49"></a>

Currently it opens a separate buffer. I would prefer a popup.

## TODO Configure the `org-edit-src-code` to use ipython completion.<a id="orgheadline50"></a>

Currently, I have code completion only working in ipython buffer.
It seems doable to configure it in the edit source block as well.

## TODO Capture results from ipython to src block.<a id="orgheadline51"></a>

To avoid manual copying between ipython buffer and source code block, I could implement an `ob-ipython-capture` function, that would add last executed
command in the ipython console to the src block.
[Keyboard macros can work cross-buffer](http://stackoverflow.com/questions/27260049/emacs-cross-file-keyboard-macro), so this could be simple keyboard macro, but I didn't try it out yet.

## TODO Figure out why SVG doesn't work<a id="orgheadline52"></a>

In order to make a svg graphic rather than png, you may specify the
output format globally to IPython.

    %config InlineBackend.figure_format = 'svg'

# Further reading<a id="orgheadline60"></a>

## [Official org mode documentation](http://orgmode.org/worg/org-tutorials/org-spreadsheet-intro.html)<a id="orgheadline54"></a>

## [Official documentation of ob-ipython](https://raw.githubusercontent.com/gregsexton/ob-ipython/master/README.org)<a id="orgheadline55"></a>

Open org directly in Emacs:

    (browse-url-emacs "https://raw.githubusercontent.com/gregsexton/ob-ipython/master/README.org")

## [Research paper: An Effective Git And Org-Mode Based Workflow For Reproducible Research](http://dl.acm.org/citation.cfm?id=2723881)<a id="orgheadline56"></a>

Search by DOI 10.1145/2723872.2723881 on sci hub.

## [Run a whole research department on CMU using org mode](http://kitchingroup.cheme.cmu.edu/blog/2014/08/08/What-we-are-using-org-mode-for/)<a id="orgheadline57"></a>

## [Org mode for managing your server configuration](http://www.howardism.org/Technical/Emacs/literate-devops.html)<a id="orgheadline58"></a>

## Manage your emacs configuration using org mode<a id="orgheadline59"></a>

<https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org>
<http://mescal.imag.fr/membres/arnaud.legrand/misc/init.php>
<http://emacs.stackexchange.com/questions/3143/can-i-use-org-mode-to-structure-my-emacs-or-other-el-configuration-file>
