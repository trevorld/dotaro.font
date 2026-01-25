desc "Build font files and copy to ~/.local/share/fonts"
task :default => "share/dotaro-narrow.ttf"
task :default => "share/dotaro-square.ttf"
task :default => "share/dotaro-narrow-code-charts.pdf"
task :default => "share/dotaro-square-code-charts.pdf"
task :default => "README.html"

file "README.html" => "README.Rmd"
task "README.html" => "share/dotaro-narrow.ttf"
task "README.html" => "share/dotaro-square.ttf"
file "README.html" do
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "knitr::knit(\"README.Rmd\")"'
    sh 'pandoc -o README.html README.md'
    sh 'Rscript -e "pkgdown::build_site()"'
end

file "share/dotaro-narrow.ttf" => Rake::FileList["R/*.r"]
file "share/dotaro-narrow.ttf" do
    sh 'Rscript share/generate_fonts.r'
    sh "cp share/dotaro-narrow.ttf ~/.local/share/fonts/"
    sh "cp share/dotaro-square.ttf ~/.local/share/fonts/"
end

file "share/dotaro-narrow-code-charts.pdf" => "share/dotaro-narrow.ttf"
file "share/dotaro-narrow-code-charts.pdf" => "share/dotaro-narrow-code-charts.Rtex"
file "share/dotaro-narrow-code-charts.pdf" do
  Dir.chdir("share") do
    sh "Rscript -e 'knitr::knit(\"dotaro-narrow-code-charts.Rtex\", \"dotaro-narrow-code-charts.xelatex\")'"
    sh "xelatex dotaro-narrow-code-charts.xelatex"
    sh "xelatex dotaro-narrow-code-charts.xelatex"
  end
end

file "share/dotaro-square-code-charts.pdf" => "share/dotaro-square.ttf"
file "share/dotaro-square-code-charts.pdf" => "share/dotaro-square-code-charts.Rtex"
file "share/dotaro-square-code-charts.pdf" do
  Dir.chdir("share") do
    sh "Rscript -e 'knitr::knit(\"dotaro-square-code-charts.Rtex\", \"dotaro-square-code-charts.xelatex\")'"
    sh "xelatex dotaro-square-code-charts.xelatex"
    sh "xelatex dotaro-square-code-charts.xelatex"
  end
end

desc "Rsync fonts to trevor.l.davis.com/share/fonts"
task :deploy => :default
task :deploy do
  sh "cp share/dotaro-narrow.ttf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-square.ttf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-narrow-code-charts.pdf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-square-code-charts.pdf ../../websites/trevorldavis.com/content/share/fonts/"
  Dir.chdir("../../websites/trevorldavis.com/") do
    sh "rake deploy"
  end
end
