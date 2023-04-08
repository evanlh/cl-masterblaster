For the last couple months I have been learning Common Lisp by prototyping a tracker-influenced MIDI sequencer. It's been a lot of fun! The UI is still much too barebones but the dream (& reason for the lofi look) is to keep prototyping until I have something that would work in a hardware form-factor. Here is a short demo of it hooked up to Waves Audio Electric88.

https://user-images.githubusercontent.com/651237/230723265-6a1e96ce-239d-43dc-8d70-3c0cbd90b665.mp4

A few ideas I'm playing around with:
- Looping on different track lengths creates evolving rhythmic textures
- Euclidean sequencing (`track-set-euclidean`) is a fun & quick way to generate ideas
- Side-by-side display of tracks with different ticks-per-bar
- UI is still keyboard* driven ala Trackers (* & in the future hopefully knob-driven)

There are some big TODOs of course:
- UI needs to update to show per-track playhead position
- Velocity & other fx columns
- Support scrolling longer track lengths
- UX for everything being done in the lisp forms on the LHS

The dual constraints (prototyping in CL while assuming it could be ported to an embedded platform) has allowed me to play with some new things:
- [Bresenham's line drawing algorithm](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm)
- Thread creation/destruction, busy waiting timers
- (Naive) parsing of BDF files. (Shoutout to [Bauhaus](https://damieng.com/typography/zx-origins/bauhaus/) for the cool fonts)
- [Euclidean rhythms](https://en.wikipedia.org/wiki/Euclidean_rhythm)
- [Macros](https://lispcookbook.github.io/cl-cookbook/macros.html)
