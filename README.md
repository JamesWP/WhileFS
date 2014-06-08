# WhileFS (Lexer / Parser / AST)
(a super simple programming language from COMP12112 UOM)
(somthing to help me learn and experiment)

### while programming language

this is a simple language with an easy to understand grammar and syntax and is [turing complete!](http://en.wikipedia.org/wiki/Turing_completeness)

```
x:=0 // assignment
if(x<=10) then (a:=1) else (a:=0) // if 
while(a<=10) do (a:=a*2) // while
```

### running!!
you can give it a go with some of the samples below

```
$ mono Program.exe "x:=10"
$ mono Program.exe "x:=10;x:=x-5"
$ mono Program.exe "
    x:=0;
    while(x<=10) do
      (x:=x+1);
    if(x<=10) then
      (fail:=1)
      else
      (fail:=0)
"
```



### dependancies
This project is in f# and so you will need the mono compiler available for download
[here](http://www.go-mono.com/mono-downloads/download.html)

### building
you can run `$ build`
or alternativeley you can run 
```
$ fsharpc Syntax.fs Parser.fs Runtime.fs Program.fs
```

you can individually compile these from left to right also

### a little explaination

there are a few steps to the above solution...
first you need to lex the file into the seperate tokens
then you need to turn the tokens into the correct syntax tree
then you can apply some runtime meaning to the tree values processing etc...
then you can run it

### disclaimer and call for help

this is by no means finished please submit ideas / pull requests and star
this is just somthing i did in some spare time because i wanted to.. nothing more



see [licence](licence.md)
