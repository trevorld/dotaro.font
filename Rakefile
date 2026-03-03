desc "Build font files and copy to ~/.local/share/fonts"
task :default => "share/dotaro-ranks.ttf"
task :default => "share/dotaro-suits.ttf"
task :default => "share/dotaro-ranks-code-charts.pdf"
task :default => "share/dotaro-suits-code-charts.pdf"
task :default => "README.html"

file "README.html" => "README.Rmd"
task "README.html" => "share/dotaro-ranks.ttf"
task "README.html" => "share/dotaro-suits.ttf"
file "README.html" do
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "knitr::knit(\"README.Rmd\")"'
    sh 'pandoc -o README.html README.md'
    sh 'Rscript -e "pkgdown::build_site()"'
end

file "share/dotaro-ranks.ttf" => Rake::FileList["R/*.R"]
file "share/dotaro-ranks.ttf" do
    sh 'Rscript share/generate_fonts.R'
    sh "cp share/dotaro-ranks.ttf ~/.local/share/fonts/"
    sh "cp share/dotaro-suits.ttf ~/.local/share/fonts/"
    sh "fc-cache -f"
end

file "share/dotaro-ranks-code-charts.pdf" => "share/dotaro-ranks.ttf"
file "share/dotaro-ranks-code-charts.pdf" => "share/dotaro-ranks-code-charts.Rtex"
file "share/dotaro-ranks-code-charts.pdf" do
  Dir.chdir("share") do
    sh "Rscript -e 'knitr::knit(\"dotaro-ranks-code-charts.Rtex\", \"dotaro-ranks-code-charts.xelatex\")'"
    sh "xelatex dotaro-ranks-code-charts.xelatex"
    sh "xelatex dotaro-ranks-code-charts.xelatex"
  end
end

file "share/dotaro-suits-code-charts.pdf" => "share/dotaro-suits.ttf"
file "share/dotaro-suits-code-charts.pdf" => "share/dotaro-suits-code-charts.Rtex"
file "share/dotaro-suits-code-charts.pdf" do
  Dir.chdir("share") do
    sh "Rscript -e 'knitr::knit(\"dotaro-suits-code-charts.Rtex\", \"dotaro-suits-code-charts.xelatex\")'"
    sh "xelatex dotaro-suits-code-charts.xelatex"
    sh "xelatex dotaro-suits-code-charts.xelatex"
  end
end

desc "Rsync fonts to trevor.l.davis.com/share/fonts"
task :deploy => :default
task :deploy do
  sh "cp share/dotaro-ranks.ttf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-suits.ttf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-ranks-code-charts.pdf ../../websites/trevorldavis.com/content/share/fonts/"
  sh "cp share/dotaro-suits-code-charts.pdf ../../websites/trevorldavis.com/content/share/fonts/"
  Dir.chdir("../../websites/trevorldavis.com/") do
    sh "rake deploy"
  end
end
