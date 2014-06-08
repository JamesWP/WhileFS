# WhileFS
##### a little project to help me understand a simple lexer / parser/ AST package
##### with a little runtime thrown in for lulz

### dependancies
This project is in f# and so you will need the mono compiler available for download
[here](http://www.go-mono.com/mono-downloads/download.html)

### building
you can run `$ fsharpc Syntax.fs Parser.fs Runtime.fs Program.fs`

you can individually compile these from left to right also

### running!!
you can give it a go with some of the samples below

```
x:=0
while(x<=10) do
  (x:=x-1)
if(x<10) then
  (fail:=1)
  else
  (fail:=0)
```
