"A small brace-format class for the US-412 end-to-end tests."
Object subclass: Sample [
    | value |

    greet [ ^'hi' ]

    value [ ^value ]

    Sample class >> make [ ^self new ]
]

"A reference + a message send, for go-to-definition."
Sample new greet
