task :default => [:install_packages, :make_link]

packages = ["bartman/git-wip", "Golevka/emacs-clang-complete-async"]

desc "Takes care of any dependencies not managed by the package manager."
task :install_packages do
  Dir.mkdir("vendor") unless Dir.exists?("vendor")
  packages.each do |package|
    repo = "https://github.com/#{package}"
    puts "Installing #{repo}..."
    `cd vendor && git clone #{repo}`
  end
  `cd vendor/emacs-clang-complete-async && make`
end

desc "Updates all packages"
task :update_packages do
  dirs = packages
  dirs.each { |path| path.slice!(/.*\//) }
  dirs.each do |dir|
    print "Updating #{dir}..."
    print "DONE\n"
    `cd vendor/#{dir} && git pull`
  end
  `cd vendor/emacs-clang-complete-async && make`
end

task :make_link do
  target = File.join(File.dirname(__FILE__), "vendor", "git-wip", "git-wip")
  link_name = File.join(Dir.home, "bin", "git-wip")
  `ln -s #{target} #{link_name}`
  `chmod u+x #{link_name}`
end
