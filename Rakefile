task :default => [:install_packages]

packages = ["Golevka/emacs-clang-complete-async"]

desc "Takes care of any dependencies not managed by the package manager."
task :install_packages do
  Dir.mkdir("vendor") unless Dir.exists?("vendor")
  packages.each do |package|
    repo = "https://github.com/#{package}"
    print "Installing #{repo}..."
    `cd vendor && git clone #{repo}`
    print "DONE\n"
  end
  `cd vendor/emacs-clang-complete-async && make`
end

desc "Updates all packages"
task :update_packages do
  dirs = packages
  dirs.each { |path| path.slice!(/.*\//) }
  dirs.each do |dir|
    print "Updating #{dir}..."
    `cd vendor/#{dir} && git pull`
    print "DONE\n"
  end
  `cd vendor/emacs-clang-complete-async && make`
end
