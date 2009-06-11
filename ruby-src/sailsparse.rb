#!/usr/bin/ruby

#pass in a Sails xhtml file and it will compile it into javascript
require "rexml/streamlistener"
require "rexml/document"

module SailsXML
  #this class find the last tag in the XML stream
  #that is at the most basic level
  class LastRootTagStreamListener
    include(REXML::StreamListener)
    attr_reader :count, :last_root_index
    def initialize
      @count = 0
      @tag_depth = 0
    end
    def tag_start(name, attrs)
      @last_root_index = @count if @tag_depth == 0
      @tag_depth = @tag_depth + 1 #increase tag depth on encountering a start tag
      @count = @count + 1
    end
    def tag_end(name)
      @tag_depth = @tag_depth - 1 #decrease on end tag
    end
  end
  class SailsXMLStreamListener
    include(REXML::StreamListener)
    def ostr ; @ostr; end
    def js_var_name ;  @jsvar ; end
    def output_linebreaks? ; nil ; end
    def linebreak
      output_linebreaks? ? "\n" : ""
    end
    JSON_ESCAPED = {
       "\010" =>  '\b',
       "\f" =>    '\f',
       "\n" =>    '\n',
       "\r" =>    '\r',
       "\t" =>    '\t',
       '"' =>     '\"',
       '\\' =>    '\\\\'
    }
    #I don't believe this works yet for all characters
    def escape_javascript(javascript)
      return javascript.gsub(/[\010\f\n\r\t"\\]/) { |s|
          JSON_ESCAPED[s]
        }
    end  
    
    def initialize(js_out_var, strm = $stdout, last_root_index = nil)
      puts "Index of Last Tag: #{last_root_index}"
      @tag_depth = 0
      @ostr = strm
      @jsvar = js_out_var
      @last_out_type = :jsexpr
      @current_tag_index = 0
      @last_root_index = last_root_index # index of the last root tag in the stream
      ostr << "#{js_var_name}=" + '""'
    end
    
    
    def rawout(str)
      ostr << "\n" + str
      @last_out_type = :rawout #debug
    end
    
    def jsexprout(str)
      if @last_out_type == :jsexpr
        ostr << "+#{linebreak}" + str
      else
        ostr << "\n" + js_var_name + "+=" + str
      end
      @last_out_type = :jsexpr
    end
    
    def jsout(str)
      jsexprout '"' + escape_javascript(str) + '"'
    end
    
    
    #stream stuff
    
    def generic_callback
      if @just_did_starttag
        jsout " >"
      end
      @just_did_starttag = false
    end
    
    def str2js_str(str)
      str.nil? ? "null" : '"'+escape_javascript(str)+'"'
    end
    
    def hash2js_str(hash)
      return 'null' if hash.nil?
      "{" + hash.keys.map {|key| "#{key} : " + str2js_str(hash[key]) }.join(',') + "}"
    end
    
    def tag_start(name, attrs)
      tagstr = "<" + name
      idexpr = Hash.new
      idexpr[:options] = Hash.new
      idexpr[:options][:first] = "true" if @current_tag_index == 0
      idexpr[:options][:last] = "true" if @current_tag_index == @last_root_index
      if @tag_depth == 0
        additional_style = "visibility:hidden;"
        if attrs["style"]
          attrs["style"] += additional_style
        else
          attrs["style"] = additional_style
        end
      end
      attrs.each do |attr|
        case attr[0]
          when /^suaveField$/i
            idexpr[:name] = attr[1]
          when /^suaveRoot$/i
            idexpr[:root] = attr[1]
            idexpr[:name] = "root"
          when /^suaveFirst$/i
            idexpr[:options][:first] = attr[1]
          when /^suaveLast$/i
            idexpr[:options][:last] = attr[1]
          when /^suaveInsertion$/i
            idexpr[:options][:insertion] = attr[1]
          when /^suaveInsertionLocation$/i
            idexpr[:options][:insertionLocation] = attr[1]
          when /^id$/i
            idexpr[:options][:id] = attr[1]
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
          else
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
        end
      end
      
      jsout tagstr
      if !idexpr.has_key?(:name)
        if idexpr[:options].has_key?(:first) && idexpr[:options].has_key?(:last)
          idexpr[:name] = "root"
        elsif idexpr[:options].has_key?(:last)
          idexpr[:name] = "anonLast"
        elsif idexpr[:options].has_key?(:first)
          idexpr[:name] = "anonFirst"
        end
      end
      if idexpr.has_key?(:name)
        jsout ' id="'
        expr = "this.defField(\""+escape_javascript(idexpr[:name]) +'"'
        expr += ','+hash2js_str(idexpr[:options]) + ')'
        jsexprout expr
        jsout '"'
      end
      @just_did_starttag = true
      @current_tag_index =  @current_tag_index +1
      @tag_depth = @tag_depth + 1 #increase tag depth on encountering a start tag
    end
    def tag_end(name)
      
      tagstr = ''
      if @just_did_starttag
        tagstr = " />"
      else
        tagstr = "</" + name + ">"
      end
      @just_did_starttag = false
      jsout tagstr
      @tag_depth = @tag_depth - 1 #decrease on end tag    
    end
    def text(value)
      generic_callback
      jsout value
      @is_empty = false
    end
    def eof
      ostr << "\n"
    end
    
  end
  
  class SyntaxException < Exception
  end
  
end

#def traverse_node(node)
#  if node.kind_of?(REXML::Element)
#    ostr <<
#    node.each do |child|
#      traverse_node(child)
#    end
#    ostr << "#{node.}>"
#  else
#    ostr << node.to_s
#  end
#  
#end
