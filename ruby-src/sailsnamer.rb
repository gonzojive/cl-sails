module Sails
  module Inflector
    #from the Rails inflector
    def self.camelize(lower_case_and_underscored_word, first_letter_in_uppercase = true)
      #yay perl
      if first_letter_in_uppercase
        lower_case_and_underscored_word.to_s.gsub(/\/(.?)/) { "::" + $1.upcase }.gsub(/(^|_)(.)/) { $2.upcase }
      else
        lower_case_and_underscored_word.first + camelize(lower_case_and_underscored_word, true)[1..-1]
      end
    end
    def self.underscore(camel_cased_word)
      #yay cryptic regex
      camel_cased_word.to_s.gsub(/::/, '/').
      gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
      gsub(/([a-z\d])([A-Z])/,'\1_\2').
      tr("-", "_").
      downcase
    end
  end

  class Namer
    def initialize(path)
      @path = path
    end
  end
end
