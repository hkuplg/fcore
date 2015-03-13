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

desc "Capitalize .sf files"
task :cap do
  Dir["**/*.sf"].each do |old_path|
    basename = File.basename(old_path)
    if ('a'..'z').include?(basename[0])
      dirname      = File.dirname(old_path)
      old_basename = File.basename(old_path)

      new_basename = basename.split("_").map(&:capitalize).join
      new_path     = File.join(dirname, new_basename)

      puts "#{dirname}/{#{old_basename} => #{new_basename}}"
      system("git mv #{old_path} #{new_path}")
    end
  end
end
