# django-test.el

Quickly execute django tests.

![](./docs/django-test-screenshot.png)

## What it does?

This package provides a function to generate commands to run specific django tests. Invoke the function anywhere on the python file, and you'll have a test command for that file. When the cursor is at the class or method name, it will detect it and generate a specific command for it. You can see it in use [here](https://streamable.com/n67u9).

## Quickstart

### Installation

``` emacs-lisp
(require 'django-test)
(define-key python-mode-map (kbd "<f7>") 'django-test-run-test-at-point)
```

### Basic Usage
- Open a django test file.
- Go to a class or method that you want to execute the test.
- Invoke `django-test-run-test-at-point` function.
- You will be prompted to insert test command. Press <kbd>RET</kbd> to accept `django-test.el` suggestion.
- The command will be executed on a new window with the help of the `compilation` mode.

On the compilation buffer, you can press <kbd>g</kbd> to re-run the tests. You can check it out more about compilation mode on emacs manual [here](https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html#Compilation) and [here](https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html#Compilation-Mode).

This package currently supports a few extra options available on [`manage.py`](https://docs.djangoproject.com/en/2.2/ref/django-admin/) to run your tests with it. You can see the list of with all the options available to customization on the below.

| Customizable Variable         | Command option                                                                              | Example               |
|-------------------------------|---------------------------------------------------------------------------------------------|-----------------------|
| `django-test-settings-module` | [`--settings`](https://docs.djangoproject.com/en/2.2/ref/django-admin/#cmdoption-settings)  | "production_settings" |
| `django-test-keepdb`          | [`--keepdb`](https://docs.djangoproject.com/en/2.2/ref/django-admin/#cmdoption-test-keepdb) | t                     |


## License

Distributed under the GNU General Public License, version 3
