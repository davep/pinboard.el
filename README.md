# pinboard.el

[![MELPA Stable](https://stable.melpa.org/packages/pinboard-badge.svg)](https://stable.melpa.org/#/pinboard)
[![MELPA](https://melpa.org/packages/pinboard-badge.svg)](https://melpa.org/#/pinboard)

## Commentary:

`pinboard.el` is an Emacs client for [Pinboard](https://pinboard.in/).

To get started, visit [your password settings
page](https://pinboard.in/settings/password) on Pinboard and get the API
token that's displayed there. Then edit `~/.authinfo` and add a line like
this:

```
machine api.pinboard.in password foo:8ar8a5w188l3
```

Once done, you can <kbd>M-x</kbd> <kbd>pinboard</kbd> <kbd>RET</kbd> and
browse your pins. Current commands available include:

| Command                   | Key            | Description                                 |
|---------------------------|----------------|---------------------------------------------|
| `pinboard-add`            | <kbd>n</kbd>   | Add a new pin                               |
| `pinboard-delete`         | <kbd>d</kbd>   | Delete the current pin                      |
| `pinboard-edit`           | <kbd>e</kbd>   | Edit the current pin                        |
| `pinboard-kill-url`       | <kbd>k</kbd>   | Add pin's URL to the `kill-ring`.           |
| `pinboard-open`           | <kbd>RET</kbd> | Open the pin's URL in a web browser         |
| `pinboard-private`        | <kbd>P</kbd>   | Show only private pins in the list          |
| `pinboard-public`         | <kbd>p</kbd>   | Show only public pins in the list           |
| `pinboard-read`           | <kbd>r</kbd>   | Show only read pins in the list             |
| `pinboard-refresh`        | <kbd>g</kbd>   | Refresh the list, showing all pins          |
| `pinboard-search`         | <kbd>/</kbd>   | Show only pins that contain the search text |
| `pinboard-extend-tagged`  | <kbd>t</kbd>   | Add a tag to the current tag filter         |
| `pinboard-tagged`         | <kbd>T</kbd>   | Show only pins with a given tag             |
| `pinboard-toggle-read`    | <kbd>R</kbd>   | Toggle the read/unread status of the pin    |
| `pinboard-unread`         | <kbd>u</kbd>   | Show only unread pins in the list           |
| `pinboard-untagged`       | <kbd>T</kbd>   | Show only pins that have no tag             |
| `pinboard-view`           | <kbd>SPC</kbd> | View the data for a pin in a window         |
| `pinboard-visit-pinboard` | <kbd>v</kbd>   | Visit the Pinboard website                  |

Commands available that aren't part of the pin list, and that you might want
to bind to keys, include:

| Command                  | Description                           |
| ------------------------ | ------------------------------------- |
| `pinboard`               | Open the Pinboard pin list            |
| `pinboard-add`           | Add a new pin to Pinboard             |
| `pinboard-add-for-later` | Prompt for a URL and add it for later |

## Org support

[Konrad Hinsen](https://gist.github.com/khinsen) has [written a handy little
add-on](https://github.com/davep/pinboard.el/issues/8) that connects
`pinboard` with `org-mode`, by providing an easy method of pulling a link
out of `pinboard` and getting it into `org-mode`. See [the gist he wrote for
the code](https://gist.github.com/khinsen/7ed357eed9b27f142e4fa6f5c4ad45dd).

## Troubleshooting

### `pinboard.el` doesn't find API token despite it being set in `.authinfo`

Check the variable `auth-sources` to ensure that Emacs is looking for
`.authinfo`. Some Emacs-frameworks set their own default (typically
enforcing gpg encryption).

If you're using `use-package`, you can append the `~/.authinfo` file to 
the `auth-sources` list like this:

```
(use-package pinboard :config (add-to-list 'auth-sources "~/.authinfo" t))
```

## TODO

Please see [the GitHub issues
list](https://github.com/davep/pinboard.el/issues) for things I plan to do.

[//]: # (README.md ends here)
