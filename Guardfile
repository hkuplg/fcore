# A sample Guardfile
# More info at https://github.com/guard/guard#readme

ghci_options =
  [ "-ignore-dot-ghci" \
  ]

guard :haskell, 
    all_on_start: true, 
    all_on_pass: true, 
    focus_on_fail: false, 
    ghci_options: ghci_options do
  watch(%r{test/.+Spec\.l?hs$})
end
