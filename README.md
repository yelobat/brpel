# brpel - BRP (Bevy Remote Protocol) for Emacs.

## Table of contents

- [Installation](#installation)
  - [Manual (Clone)](#manual-clone)
  - [Using straight.el](#straightel)
  - [Doom Emacs](#doom-emacs)
- [Examples](#examples)  
- [License](#license)

## Installation

### Manual (Clone)

Execute the following in your terminal:

``` shell
git clone https://github.com/yelobat/brpel.git ~/.emacs.d/brpel
```

Add the directory to your Emacs `load-path` and require the package in your init file:

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/brpel")
(require 'brpel)
```

Use `C-c C-e (elisp-eval-region-or-buffer)` or simply restart Emacs for the changes to take effect.

### **`straight.el`**

If you have `straight.el`, simply add the repo via `straight-use-package`:

``` emacs-lisp
(straight-use-package
 '(brpel
   :type git
   :host github
   :repo "yelobat/brpel"))
(require 'brpel)
```

Use `C-c C-e (elisp-eval-region-or-buffer)` or simply restart Emacs for the changes to take effect.

### Doom Emacs

Add the following to your packages.el file:

``` emacs-lisp
(package! brpel
  :recipe (:host github :repo "yelobat/brpel"))
```

And the following to your config.el:

``` emacs-lisp
(use-package! brpel)
```

Run `doom sync` and restart Emacs for the changes to take effect.

## Examples

## License

This project is released under the `MIT License`. See `LICENSE` for more details.
