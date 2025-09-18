# brpel - BRP (Bevy Remote Protocol) for Emacs.

## Table of contents

- [Installation](#installation)
  - [Manual (Clone)](#manual-clone)
  - [Using straight.el](#straightel)
  - [Doom Emacs](#doom-emacs)
- [Examples](#examples)
- [Browser](#browser)
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

For Bevy v0.17.0, here are some examples on how to use them:

### `brpel-world-get-components`

``` emacs-lisp
(brpel-world-get-components 8589934559 ["bevy_sprite::text2d::Text2d"])
```

### `brpel-world-query`

``` emacs-lisp
;; An example getting all Entities that have a Transform component, and also querying with
;; for each retrieved components as to whether they have a Text2d component.
(brpel-world-query `((components . ["bevy_transform::components::transform::Transform"])
                     (option . [])
                     (has . ["bevy_sprite::text2d::Text2d"]))
                   `((with . []) (without . [])))

;; An example getting all Entities and all of their Reflectable components.
(brpel-world-query `((components . [])
                     (option . "all")
                     (has . []))
                   `((with . []) (without . [])))
```

### `brpel-world-spawn-entity`

``` emacs-lisp
(brpel-world-spawn-entity `(("bevy_sprite::text2d::Text2d" . "Hi from brpel!")))
```

### `brpel-world-despawn-entity`
``` emacs-lisp
(brpel-world-despawn-entity 8589934559)
```

### `brpel-world-remove-components`
``` emacs-lisp
(brpel-world-remove-components 8589934559 ["bevy_transform::components::transform::Transform"])
```

### `brpel-world-insert-components`
``` emacs-lisp
(brpel-world-insert-components 8589934559 ["bevy_transform::components::transform::Transform"])
```

### `brpel-world-mutate-components`
``` emacs-lisp
(brpel-world-mutate-components 8589934559
    "bevy_transform::components::transform::Transform"
    "translation" 
    [-50.0 -20.0 0.0])
(brpel-world-mutate-components 8589934559 
    "bevy_transform::components::transform::Transform"
    "rotation"
    [0.0 0.0 0.0 1.0])
(brpel-world-mutate-components 8589934559 
    "bevy_transform::components::transform::Transform"
    "scale"
    [1.0 1.0 1.0])
```

### `brpel-world-reparent-entites`

``` emacs-lisp
(brpel-world-reparent-entities 8589934559)
```

### `brpel-world-list-components`

``` emacs-lisp
(brpel-world-list-components)
```

### `brpel-world-get-component+watch`
``` emacs-lisp
(switch-to-buffer-other-window (brpel-world-get-components+watch 8589934559 
    ["bevy_transform::components::transform::Transform" "bevy_sprite::text2d::Text2d"]))
```

### `brpel-world-list-components+watch`

``` emacs-lisp
(switch-to-buffer-other-window (brpel-world-list-components+watch 8589934559))
```

### `brpel-world-get-resources`

``` emacs-lisp
(brpel-world-get-resources "bevy_camera::clear_color::ClearColor")
```

### `brpel-world-insert-resources`

``` emacs-lisp
(brpel-world-get-resources "bevy_camera::clear_color::ClearColor")
```

### `brpel-world-remove-resources`

``` emacs-lisp
;; This doesn't have any practical applications (from what I'm aware of), but this just an example.
(brpel-world-remove-resources "bevy_camera::clear_color::ClearColor")
```

### `brpel-world-mutate-resources`

``` emacs-lisp
;; Change the ClearColor of the bevy application to be black.
(brpel-world-mutate-resources "bevy_camera::clear_color::ClearColor"
                              "0" `
                              ((Srgba . ((red . 0.0)
                                         (green . 0.0)
                                         (blue . 0.0)
                                         (alpha . 1.0)))))
```

### `brpel-world-list-resources`

``` emacs-lisp
(brpel-world-list-resources)
```

### `brpel-registry-schema`

``` emacs-lisp
(brpel-registry-schema)
```

### `brpel-rpc-discover`

``` emacs-lisp
(brpel-rpc-discover)
```

### synchronous functions 

Each function has a synchronous version. Just add `-synchronous` to the end of the function.

## Browser

### Notice

This feature is a work in progress.

The `brpel` package now supports an ECS browser in the form of a dired-like
interface. It allows you to navigate the Resources and Entities currently stored
inside of your Bevy app through navigating a similar interface to dired.
Resources and Entities are simulated as directories and values are represented
as files.

## License

This project is released under the `MIT License`. See `LICENSE` for more details.
