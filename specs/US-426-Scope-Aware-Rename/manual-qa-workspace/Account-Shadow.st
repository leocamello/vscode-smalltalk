"Account-Shadow.st — a method whose ARGUMENT is named `balance`, shadowing the
instance variable. Renaming the ivar must NOT touch this method (its `balance` is
a local). This is the shadowing-safety case."

Account extend [
    scaleBy: balance [ ^balance * 2 ]
]
