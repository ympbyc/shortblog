SHORTBLOG
=========

Super simple file-based CLI-oriented miniblog written in Common LISP.

It is set up for my own use at this stage. It's meant to be hacked to suit your need.

![example diary](https://raw.githubusercontent.com/ympbyc/shortblog/gh-pages/blog/2021-03/thumbs/shot-2021-03-12_22-15-14.jpg)

Install
-------

```
git clone [this repo]
rm -rf ./blog/**
cd shortblog
./build.sh
chmod u+x shortblog
#[move shortblog under your $PATH]
```

Usage
-----

```
shortblog make-post "I wrote the README for SHORTBLOG"
shortblog add-media /mnt/DCIM/xxx/IMGP0000.JPG
for img in /mnt/DCIM/xxx/*.JPG; do shortblog add-media "$img"; done
shortblog show-posts | more
shortblog save-html
shortblog share 2022-05-06    #create a html for single day (for sharing)
```

etc.


Requirement
-----------

+ SBCL
+ quicklisp (for uiop, ppcre)
+ [buildapp](https://www.xach.com/lisp/buildapp)
+ Imagemagick
+ facedetect

tested on linux only.


OK to Copy, Modify, Use. Please visibly credit Minori Yamashita.
