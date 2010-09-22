require 'fileutils'

# Files
def entries
  @files ||= Dir.entries( File.expand_path( '~/.dotfiles' ) ) - $exclude
end

# Files and folders which shouldn't be copied over
$exclude = [
  '.',
  '..',
  '.git',
  '.gitignore',
  'bootstrap.sh',
  'Gemfile',
  'Gemfile.lock',
  'Rakefile',
  'README.md'
]

desc 'Backup previous dotfiles.'
task :backup do
  dir = FileUtils.mkdir_p( File.join( File.expand_path( '~' ), '.dotfiles-backup', "#{Time.now}" ) )[0]
  entries.each do | file |
    orig = File.expand_path( "~/#{file}" )
    FileUtils.mv orig, "#{dir}/#{file}" if File.exists? orig
  end
end

desc 'Update dotfiles repository.'
task :update do
  system 'git pull'
end

desc 'Run all install tasks in order.'
task :install => [ 'install:deps', 'install:copy', 'install:post' ]

namespace :install do

  desc 'Check for and install required dependencies.'
  task :deps do
    puts 'Please install bundler and re-run installation. http://gembundler.com/' and exit 1 unless system 'which bundle'
    system 'bundle install'
  end

  desc 'Copy dotfiles over to home dir.'
  task :copy do
    entries.each do | file |
      FileUtils.cp_r file, File.expand_path( "~/#{file}" ), :verbose => true, :remove_destination => true
    end
  end

  desc 'Run post-install tasks.'
  task :post do
    # Command T
    Dir.chdir File.expand_path( '~/.vim/bundle/command-t' ) do
      system 'rake make'
    end
    puts "Don't forget to edit your '~/.gitconfig'!!!"
  end

end
