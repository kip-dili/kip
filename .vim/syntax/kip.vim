" Quit when a syntax file was already loaded.
if exists('b:current_syntax') | finish|  endif

syntax keyword kipKeyword Bir bir ya da olabilir var olamaz değilse yazdır diyelim olsun olarak yerleşik
syn region   kipEncl transparent matchgroup=kipKeyword start="(" matchgroup=kipKeyword end=")" contains=ALLBUT,kipParenErr
syn region   kipComment start="(\*" end="\*)" contains=@Spell,kipComment,kipTodo

hi def link kipComment	   Comment
hi def link kipKeyword	   Keyword
hi def link kipTypeName   Type
hi def link kipCtorName   Constant

let b:current_syntax = 'kip'
