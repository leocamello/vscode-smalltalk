"
    File: 04_comments.st
    Purpose: Test cases for comment variations.
"

" A simple comment on its own line. "

1 + 1. " A comment at the end of a line of code. "

" Another comment. " " And another one on the same line (less common). "

"
A comment that
spans multiple
lines.
"

" An empty comment: " "".

" Comment before any code "
Object new.

" Comment in the middle of statements "
Transcript show: 'Before comment'.
" This is a comment between statements. "
Transcript show: 'After comment'.

" Comment containing special characters: !@#$%^&*()_+{}|:<>?~`-=[]\;',./ "
" Comment with 'single quotes' and ""double quotes"" inside (escaped by context). "

" Comment at the very end of the file "
