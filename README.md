# pinboard.el

The start of an Emacs client for [Pinboard](https://pinboard.in/).

Currently the early days of a work in progress. As such, if you play with it
right now, don't be shocked if something is missing or plain broken.

To get started, visit [your password settings
page](https://pinboard.in/settings/password) on Pinboard and get the API
token that's displayed there. Then, in Emacs, set `pinboard-api-token` to
it. Eg, in your `*scratch*` type:

```elisp
(setq pinboard-api-token "foo:8ar8a5w188l3")
```

And hit <kbd>Ctrl-x</kbd> <kbd>Ctrl-e</kbd> after it. (in time, I'll have a
far more sensible way to set and remember the token -- for now though I
don't want to be encouraging people to accidentally set it in a way that it
ends up in their public repository for the Emacs config, etc).

Once done, you can <kbd>M-x</kbd> <kbd>pinboard</kbd> <kbd>RET</kbd> and
browse your pins. The client is currently very feature-poor, but expanding
as I get time to work on it. Current commands available in the list include:

| Command            | Key | Description                         |
|--------------------|-----|-------------------------------------|
| `pinboard-refresh` | g   | Refresh the list, showing all pins  |
| `pinboard-unread`  | u   | Show only unread pins in the list   |
| `pinboard-view`    | v   | View the data for a pin in a window |
| `pinboard-open`    | RET | Open the pin's URL in a web browser |

The code only works with all pins right now, and does its best to ensure
that the list is never requested more than once every 5 minutes (as per the
rate limits in the Pinboard API). Expect this to go wrong.

[//]: # (README.md ends here)
