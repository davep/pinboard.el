# pinboard.el

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

| Command                  | Key            | Description                                 |
|--------------------------|----------------|---------------------------------------------|
| `pinboard-add`           | <kbd>n</kbd>   | Add a new pin                               |
| `pinboard-delete`        | <kbd>d</kbd>   | Delete the current pin                      |
| `pinboard-edit`          | <kbd>e</kbd>   | Edit the current pin                        |
| `pinboard-kill-url`      | <kbd>k</kbd>   | Add pin's URL to the `kill-ring`.           |
| `pinboard-open`          | <kbd>RET</kbd> | Open the pin's URL in a web browser         |
| `pinboard-private`       | <kbd>P</kbd>   | Show only private pins in the list          |
| `pinboard-public`        | <kbd>p</kbd>   | Show only public pins in the list           |
| `pinboard-read`          | <kbd>r</kbd>   | Show only read pins in the list             |
| `pinboard-refresh`       | <kbd>g</kbd>   | Refresh the list, showing all pins          |
| `pinboard-search`        | <kbd>/</kbd>   | Show only pins that contain the search text |
| `pinboard-extend-tagged` | <kbd>t</kbd>   | Add a tag to the current tag filter         |
| `pinboard-tagged`        | <kbd>T</kbd>   | Show only pins with a given tag             |
| `pinboard-toggle-read`   | <kbd>R</kbd>   | Toggle the read/unread status of the pin    |
| `pinboard-unread`        | <kbd>u</kbd>   | Show only unread pins in the list           |
| `pinboard-untagged`      | <kbd>T</kbd>   | Show only pins that have no tag             |
| `pinboard-view`          | <kbd>SPC</kbd> | View the data for a pin in a window         |

## TODO

Please see [the GitHub issues
list](https://github.com/davep/pinboard.el/issues) for things I plan to do.

[//]: # (README.md ends here)
