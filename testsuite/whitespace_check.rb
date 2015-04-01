# -*- coding: utf-8 -*-
where     = ["**/*.hs", "**/*.sf"]
excluding = /\/(Lexer.hs|Parser.hs)$/

tab                 = [/\t/,   "\033[41m»»\033[0m"]
trailing_whitespace = [/\s+$/, "\033[41m¬\033[0m"]

checklist = [tab, trailing_whitespace]
checklist_description = "hard tabs or trailing whitespaces"

test_failed = false

Dir[*where].each do |path|
  next if path.match(excluding)

  File.open(path, "r") do |f|
    f.each_line.with_index do |line, i|
      line.chomp!
      line_number = i + 1
      bad_line = false

      checklist.each do |pat, replacement|
        if line.match(pat)
          bad_line = true; test_failed = true
          line.gsub!(pat, replacement)
        end
      end

      puts "#{path}:#{line_number} #{line}" if bad_line
    end
  end
end

if test_failed
  $stderr.puts "\n\tFailed the whitespace test\n\tDo not include #{checklist_description}\n\n"
  exit 1
else
  exit 0
end
