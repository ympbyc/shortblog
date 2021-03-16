SHORTBLOG
=========

Super simple file-based CLI-oriented miniblog written in Common LISP.

It is set up for my own use at this stage. Please apropreate core.lisp for your situation.

![example diary](https://raw.githubusercontent.com/ympbyc/shortblog/gh-pages/blog/2021-03/thumbs/shot-2021-03-12_22-15-14.jpg)

Usage
-----

```
shortblog make-post "I wrote the README for SHORTBLOG"
shortblog add-media /mnt/DCIM/xxx/IMGP0000.JPG
for img in /mnt/DCIM/xxx/*.JPG; do shortblog add-media "$img"; done
shortblog show-posts | more
shortblog save-html
```

etc.


Requirement
-----------

+ SBCL
+ quicklisp (for uiop, ppcre)
+ [buildapp](https://www.xach.com/lisp/buildapp)
+ Imagemagick

tested on linux only.


OK to Copy, Modify, Use. Please visibly credit Minori Yamashita.
