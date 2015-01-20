desc "Count the lines of code (aka LOC)"
task :cloc do
  system("rm -f compiler/{Lexer,Parser}.hs")
  system("cloc compiler")
end

desc "Clean Java source files in {examples/,lib/}"
task :clean do
  system("rm -f examples/*.java")
  system("rm -f lib/*.java")
end
