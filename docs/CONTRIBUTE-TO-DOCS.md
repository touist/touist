
I chose to try to write the Touist documentation using something
a little more confortable than the good-and-old latex. The language
is called Madoko, it is close to markdown (so very readable) and there
is an online editor (madoko.net/editor.html).

If you want to edit the .mdk file, you have multiple choices:

- (using the nice madoko GUI locally) either you do `npm install -g madoko-local`
  and you run the madoko webserver with `madoko-local --port 8080 --run -l` from
  the `docs` directory and you Open... Local Disk... and you select the .mdk file.
  To get your changes reviewed and accepted, fork the project, commit your changes
  and submit a pull request.

- (using the online madoko GUI and github account) 
  You first have to fork the touist project: https://github.com/touist/touist/fork
  Then, go to https://www.madoko.net/editor.html and log into your github account.
  Select the project freshly forked with 
      "Open..." -> "Github" -> "yourname/touist" -> "master" -> "docs/" ->
  Then, you can open the .mdk.
  To submit the changes, do "Save to...".
  Warning: there seems to be a bug that prevents to choose a directory AND
  a filename.mdk that are different. For now, choosing the directory "docs/"
  will result to creating "docs.mdk" instead of reference-manual.mdk.


Do not put the HTML or PDF into github. The HTML and PDF files are generated
by the circle.yml on circleci.com and then published on
    http://touist.github.io/

WARNING: it will be published onto http://touist.github.io once the changes are
pulled into the master branch. 

Known errors with madoko
========================

With the error:
    
    reference-manual: analyse and embed math images.
    error: cannot read: out/math/reference-manual-math-plain-1.svg
    fs.js:681
    return binding.rename(pathModule._makeLong(oldPath),
                    ^
This error has been reported on the madoko website:
http://madoko.codeplex.com/workitem/162

The actual errors doesn't show. But you can show the error with `madoko -vvvv`

    "dvisvgm" -n -b0.2pt -e -j -v3 -d3 -p1-40 -o "math/reference-manual-math-plain-%1p.svg" "reference-manual-math-plain.xdv"

    DVI error: DVI format 7 not supported

This error only happens with dvisvgm v1.15.1 (shipped with Texlive 2016), and does not
occur with dvisvgm 1.9.2 (shipped with Texlive 2015). This "Dvi format 7" error is
disscussed here: https://github.com/mgieseki/dvisvgm/issues/59

