$LOAD_PATH << File.expand_path(File.dirname(__FILE__))
puts File.expand_path(File.dirname(__FILE__))
require 'erb'
require 'fileutils'
require 'pathname'
require 'sailsnamer'

SAILS_PATH = File.expand_path(File.dirname(__FILE__)) + "/"
TEMPLATES_PATH = "#{SAILS_PATH}templates-paren/"
module Sails
  #helps generate sails
  module Skeleton
    def templates_root
      TEMPLATES_PATH
    end
    def template_for(template_type)
      fname = nil
      fext = template_type == :view_html ? "rhtml" : "rjs"
      case template_type
        when :view_html
          fname = templates_root + "view.template." + fext
        else
          fname = templates_root + template_type.to_s.downcase + ".template." + fext
      end
      File.new(fname, "r").read
    end
    class Generic
      include(Skeleton)
      def initialize(classname)
        @classname = classname
      end
      def template_symbol
        split_along_modules = self.class.name.split("::")
        Inflector::underscore(split_along_modules.last).downcase.to_sym
      end
      def result
        begin
          ERB.new(template_for(template_symbol)).result(binding)
        rescue Errno::ENOENT
          puts "No template found for #{template_symbol.to_s}"
        end
      end
    end
    class View < Generic
    end
    class ViewHtml < Generic
    end
    class Controller < Generic
    end
    class Model < Generic
    end
  end
  
  class Generator
    def initialize(basename)
      @pathstr = basename
    end
    
    def run()
      givenpath = Pathname.new(@pathstr)
      @classname = Inflector::camelize(givenpath.basename.to_s)
      FileUtils::mkdir_p(givenpath)
      
      genfile(:view, format_file_path(givenpath, "View"))
      genfile(:view_html, format_file_path(givenpath, "View", "html"))
      genfile(:controller, format_file_path(givenpath, "Controller"))
      genfile(:model, format_file_path(givenpath, "Model"))
    end
    
    def format_file_path(givenpath, suffix = nil, extension = "js")
      suffix = suffix.nil? ? "" : '.' + suffix.downcase + "." + extension
      Pathname.new((givenpath + givenpath.basename.to_s).to_s.downcase + suffix)
    end
    
    def genfile(type, path)
      if path.exist?
        puts "#{path} exists, skipping."
      else
        File.new(path, "w").write(gencomponent(type))
        puts "#{path} created."
      end
    end
    
    def gencomponent(type)
      Skeleton.const_get(Inflector::camelize(type.to_s).to_sym).new(@classname).result
    end
  end
end

if ARGV.empty?
  puts "Must supply argument of file to generate"
  exit
end
puts "Generating #{ARGV[0]}..."
generator = Sails::Generator.new(ARGV[0])
generator.run()
puts "Finished"
