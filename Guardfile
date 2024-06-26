guard 'process', name: 'Shiny', command: ['Rscript', "dev/run_dev.R"] do
  watch(%r{./.+\.R$})
end

guard 'livereload', grace_period: 0.5 do
  watch(%r{./.+\.R$})
end
