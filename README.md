# efl-pas
[Enlightenment Foundation Libraries](https://www.enlightenment.org/about-efl) bindings in [FreePascal](https://www.freepascal.org)

## Other bindings for [EFL](https://www.enlightenment.org/about-efl)

- [Python](https://docs.enlightenment.org/python-efl/current)
- [Rust](https://codeberg.org/JustSoup321/efl-rs)
- [Vala](https://github.com/freesmartphone/libeflvala)

## Dependencies

- efl-devel >= 1.28
- libclang  >= 18.0
- pkg-config

## Quickstart

```shell
fpc src/example.pas
```

## Work in process

- [ ] Ecore
  - [ ] [Con](https://docs.enlightenment.org/python-efl/current/ecore/module-ecore_con.html)
  - [ ] Event
  - [ ] [Timer](https://docs.enlightenment.org/python-efl/current/ecore/class-timer.html)
- [ ] [Emotion](https://docs.enlightenment.org/python-efl/current/emotion/emotion.html)
- [ ] Elementary
  - [x] [Application](https://www.enlightenment.org/develop/legacy/api/c/start#infralist.html)
  - [ ] [Containers](https://www.enlightenment.org/develop/legacy/api/c/start#containerslist.html)
    - [x] [Box](https://docs.enlightenment.org/python-efl/current/elementary/box.html)
    - [ ] [Bubble](https://docs.enlightenment.org/python-efl/current/elementary/bubble.html)
    - [ ] [Frame](https://docs.enlightenment.org/python-efl/current/elementary/frame.html)
    - [ ] [Flip](https://docs.enlightenment.org/python-efl/current/elementary/flip.html)
    - [ ] [Hover](https://docs.enlightenment.org/python-efl/current/elementary/hover.html)
    - [ ] [NaviFrame](https://docs.enlightenment.org/python-efl/current/elementary/naviframe.html)
    - [ ] [Notify](https://docs.enlightenment.org/python-efl/current/elementary/notify.html)
    - [ ] [Layout](https://docs.enlightenment.org/python-efl/current/elementary/layout.html)
    - [ ] [Panel](https://docs.enlightenment.org/python-efl/current/elementary/panel.html)
    - [ ] [Panes](https://docs.enlightenment.org/python-efl/current/elementary/panes.html)
    - [ ] [Popup](https://docs.enlightenment.org/python-efl/current/elementary/popup.html)
    - [ ] [Scroller](https://docs.enlightenment.org/python-efl/current/elementary/scroller.html)
    - [ ] [Window](https://docs.enlightenment.org/python-efl/current/elementary/window.html)
  - [x] [Widgets](https://www.enlightenment.org/_legacy_embed/widgetslist.html)
    - [ ] Rangers
      - [ ] [Spinner](https://docs.enlightenment.org/python-efl/current/elementary/spinner.html)
      - [ ] [Slider](https://docs.enlightenment.org/python-efl/current/elementary/slider.html)
    - [ ] Selectors
      - [ ] [ActionSlider](https://docs.enlightenment.org/python-efl/current/elementary/actionslider.html)
      - [ ] [Calendar](https://docs.enlightenment.org/python-efl/current/elementary/calendar.html)
      - [ ] [FileSelector](https://docs.enlightenment.org/python-efl/current/elementary/fileselector.html)
      - [ ] [DaySelector](https://docs.enlightenment.org/python-efl/current/elementary/dayselector.html)
      - [ ] [DiskSelector](https://docs.enlightenment.org/python-efl/current/elementary/diskselector.html)
      - [ ] [ColorSelector](https://docs.enlightenment.org/python-efl/current/elementary/colorselector.html)
      - [ ] [Ctxpopup](https://docs.enlightenment.org/python-efl/current/elementary/ctxpopup.html)
      - [ ] [Combobox](https://docs.enlightenment.org/python-efl/current/elementary/combobox.html)
      - [ ] [FlipSelector](https://docs.enlightenment.org/python-efl/current/elementary/flipselector.html)
      - [ ] [HoverSel](https://docs.enlightenment.org/python-efl/current/elementary/hoversel.html)
      - [ ] [Menu](https://docs.enlightenment.org/python-efl/current/elementary/menu.html)
      - [ ] [List](https://docs.enlightenment.org/python-efl/current/elementary/list.html)
      - [ ] [Radio](https://docs.enlightenment.org/python-efl/current/elementary/radio.html)
      - [ ] [SegmentControl](https://docs.enlightenment.org/python-efl/current/elementary/segment_control.html)
      - [ ] [Toolbar](https://docs.enlightenment.org/python-efl/current/elementary/toolbar.html)
    - [ ] Triggers
      - [x] [Button](https://docs.enlightenment.org/python-efl/current/elementary/button.html)
      - [ ] [Clock](https://docs.enlightenment.org/python-efl/current/elementary/clock.html)
    - [ ] Input
      - [x] [Entry](https://docs.enlightenment.org/python-efl/current/elementary/entry.html)
    - [ ] Output
      - [ ] [Image](https://docs.enlightenment.org/python-efl/current/elementary/image.html)
      - [x] [Label](https://docs.enlightenment.org/python-efl/current/elementary/label.html)
      - [ ] [Separator](https://docs.enlightenment.org/python-efl/current/elementary/separator.html)
      - [ ] [ProgressBar](https://docs.enlightenment.org/python-efl/current/elementary/progressbar.html)
    - [ ] Misc
      - [ ] [Icon](https://docs.enlightenment.org/python-efl/current/elementary/icon.html)
      - [ ] [Cursor](https://docs.enlightenment.org/python-efl/current/elementary/cursor.html)
      - [ ] [Tooltip](https://docs.enlightenment.org/python-efl/current/elementary/tooltip.html)
