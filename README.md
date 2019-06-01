# pinboard.el

## Commentary:

`pinboard.el` is an Emacs client for [Pinboard](https://pinboard.in/).

Currently the early days of a work in progress. As such, if you play with it
right now, don't be shocked if something is missing or plain broken.

To get started, visit [your password settings
page](https://pinboard.in/settings/password) on Pinboard and get the API
token that's displayed there. Then edit `~/.authinfo` and add a line like
this:

```
machine api.pinboard.in password foo:8ar8a5w188l3
```

Once done, you can <kbd>M-x</kbd> <kbd>pinboard</kbd> <kbd>RET</kbd> and
browse your pins. The client is currently very feature-poor, but expanding
as I get time to work on it. Current commands available in the list include:

| Command             | Key            | Description                                 |
|---------------------|----------------|---------------------------------------------|
| `pinboard-refresh`  | <kbd>g</kbd>   | Refresh the list, showing all pins          |
| `pinboard-kill-url` | <kbd>k</kbd>   | Add pin's URL to the `kill-ring`.           |
| `pinboard-unread`   | <kbd>u</kbd>   | Show only unread pins in the list           |
| `pinboard-read`     | <kbd>r</kbd>   | Show only read pins in the list             |
| `pinboard-tagged`   | <kbd>t</kbd>   | Show only pins that have a given tag        |
| `pinboard-untagged` | <kbd>T</kbd>   | Show only pins that have no tag             |
| `pinboard-add`      | <kbd>n</kbd>   | Add a new pin                               |
| `pinboard-edit`     | <kbd>e</kbd>   | Edit the current pin                        |
| `pinboard-search`   | <kbd>/</kbd>   | Show only pins that contain the search text |
| `pinboard-view`     | <kbd>SPC</kbd> | View the data for a pin in a window         |
| `pinboard-public`   | <kbd>p</kbd>   | Show only public pins in the list           |
| `pinboard-private`  | <kbd>P</kbd>   | Show only private pins in the list          |
| `pinboard-open`     | <kbd>RET</kbd> | Open the pin's URL in a web browser         |

The code only works with all pins right now, and does its best to ensure
that the list is never requested more than once every 5 minutes (as per the
rate limits in the Pinboard API). Expect this to go wrong.

## TODO

The initial release is all about getting viewing and reading of pins up and
going. Here's what I planning on doing as time goes on:

- [ ] View tag list/cloud.
- [X] Add a pin.
- [ ] Have add locally update the list.
- [X] Edit a pin.
- [ ] Have edit locally update the list.
- [ ] Delete a pin.
- [ ] Have delete locally update the list.
- [ ] Mark unread pin as read.
- [ ] Notes viewing.
- [ ] Stuff.

[//]: # (README.md ends here)
