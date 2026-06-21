"A small brace-format class for the US-412 end-to-end tests."
Object subclass: Sample [
    | value |

    greet [
        ^'hi'
    ]

    value [ ^value ]

    Sample class >> make [ ^self new ]
]

"A reference + two message sends, for go-to-definition and document highlight."
Sample new greet.
Sample new greet
