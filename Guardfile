ghci_options =
  [ "-ignore-dot-ghci" \
  ]

guard :haskell, ghci_options: ghci_options,
    all_on_start: true, 
    all_on_pass: true, 
    focus_on_fail: false do
  watch(%r{compiler/.+\.l?hs$})
  watch(%r{test/.+Spec\.l?hs$})
end
