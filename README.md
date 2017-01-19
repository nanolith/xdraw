xdraw utility
=============

The xdraw utility is a simple image generator created to build diagrams from
simple scripts in one of several domain-specific languages.

swim lane
---------

The swim lane diagram is a simple Rummler-Brache style diagram in which a
process flow is described through different contexts.  Each context gets its own
"swim lane", and the interaction between different contexts is shown using
arrows.

A file with the extension `.swimlane` will automatically be parsed as a swim
lane diagram.  The following example syntax creates a swim lane diagram with
four lanes.

    swimlanes {
        lane "Client" {
            style laneStyle
    
            C1: box { text "Client initiates request" }
            C2: box { text "Client waits..." }
            C3: box { text "Client receives response" }
        }
    
        lane "Proxy" {
            style laneStyle
    
            P1: box { text "Proxy forwards request" }
            P2: box { text "Proxy waits..." }
            P3: box { text "Proxy forwards response" }
        }
    
        lane "Server" {
            style laneStyle
    
            S1: box { text "Server receives request" }
            S2: box { text "Server processes request" }
            S3: box { text "Server sends response" }
        }
    
        //vertical paths
        path { style contextStyle C1 --> C2 --> C3 }
        path { style contextStyle P1 --> P2 --> P3 }
        path { style contextStyle S1 --> S2 --> S3 }
    
        //horizontal paths
        path { style sendStyle C1 --> P1 --> S1 }
        path { style receiveStyle S3 --> P3 --> C3 }
    
        style laneStyle {
            width 100
        }
    
        style contextStyle {
            color black
            line solid[3]
            begin straight
            end arrow.small
        }
    
        style sendStyle {
            color red
            line solid[3]
            begin straight
            end arrow.small
        }
    
        style receiveStyle {
            color red
            line solid[3]
            begin straight
            end arrow.small
        }
    }

This example shows how swim lanes can be created, items within the swim lanes
can be added with labels, paths between the labels can be defined, and styles
can be enhanced.

blocks
------

The blocks DSL allows block diagrams to be created.  Individual blocks can be
defined with a flow that can either be left-right, up-down, right-left, or
down-up.  Blocks can be styled with or without borders.  Arrows can be connected
to blocks.  A file with the extension `.blocks` will automatically be parsed as
a blocks diagram.

This example demonstrates a linked list in blocks format.

    blocks {
        dpi 300
    
        block {
            style open
    
            //first list node and template for all other nodes
            N1: block {
                style listNode
    
                N1A: block {
                    style A
                    text "H"
                }
    
                N1D: block {
                    style D
    
                    N1Link: circle { style link }
                }
            }
    
            N2: clone(N1, "l/N1/N2/") {
                N2A: clone(N1A) {
                    text "E"
                }
            }
    
            N3: clone(N1, "l/N1/N3/") {
                N3A: clone(N1A) {
                    text "L"
                }
            }
    
            N4: clone(N1, "l/N1/N4/") {
                N4A: clone(N1A) {
                    text "L"
                }
            }
    
            N5: clone(N1, "l/N1/N5/") {
                N5A: clone(N1A) {
                    text "O"
                }
            }
    
            NULL: block {
                style open
    
                text "0"
            }
    
            A1: arrow {
                style listLink
    
                N1Link --> N2
            }
    
            A2: clone(A1, "l/N2/N3/" "l/N1/N2/") { }
            A3: clone(A1, "l/N2/N4/" "l/N1/N3/") { }
            A4: clone(A1, "l/N2/N5/" "l/N1/N4/") { }
        }
    
        style open {
            flow left-right
            border none
    
            spacing 0.5in
        }
    
        style listNode {
            flow left-right
            border solid[1]
        }
    
        style A {
            flow singleton
            border solid[1]
            width 80%
        }
    
        style link {
            color green
            border solid[1]
            fill true
            width 20%
        }
    
        style listLink {
            color green
            line solid[2]
            begin straight
            end arrow.small
        }
    }

Of note is the `clone()` function, which provides a simple way to make small
changes to the copy of an already defined element.  The function's first
argument is the label of the element to be cloned.  All subsequent arguments are
modifier strings.  The `l` command performs a regular expression
search-and-replace on a given label name, replacing any matching subexpression
with the given replacement.  The `d*` command family can be used to delete
elements within a clone.  For instance, `dl/N1/` would delete any element with a
label that matches `N1`.

Labels are lazily scoped.  Labels can shadow other labels, but only at the scope
at which the label was defined or lower.  Otherwise, the first defined label at
a given scope will resolve a label reference.  A warning will be generated if
labels are ambiguous (more than one defined at the same scope).  By default, a
warning will be generated if a label shadows another label.
The `--shadow-labels` option can be specified on the command-line if this
warning is not desired.

asciiart
--------

The asciiart DSL creates an image from ascii art.  The flow of the asciiart DSL
is left-to-right and up-to-down.  Any fixed-with / monospace font can be
selected for the overall style.  This font will be inherited by all elements of
the image.  Individual text elements can be added, and these will be appended.
Lines can be ended with the `endline` keyword, or by including a `\n` in text.
A file with the extension `.asciiart` will automatically be parsed as an
asciiart diagram.

This example demonstrates an asciiart format image.

    asciiart {
      font "Lucida Console"
      background-color white
      color yellow
    
      text { "        ____________________                    \n" }
      text { "       |\                   \      l____        \n" }
      text { "       | \___________________\     |\   \       \n" }
      text { "       | |                    |    |\l___\___   \n" }
      text { "  [__]_[ |                    |[\\]| |__|_\__\  \n" }
      text { " /\[__]\ |                    |\[\\]\|. | |===\ \n" }
      text { " \ \[__]\[____________________] \[__]|__|..___] \n" }
      text { "  \/.-.\_______________________\/.-.\____\/.-.\ \n" }
      text { "   ( @ )                        ( @ )  =  ( @ ) \n" color black }
      text { "    `-'                          `-'       `-'  \n" color black }
    }
