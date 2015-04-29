module Utils
  class JSONWithIndifferentAccess
    def self.load(string)
      indifferent_access(JSON.load(string))
    end

    def self.dump(obj)
      JSON.dump(obj)
    end

    private

    def self.indifferent_access(obj)
      if obj.is_a? Hash
        obj.with_indifferent_access
      elsif obj.is_a? Array
        obj.map! { |o| indifferent_access(o) }
      else
        obj
      end
    end
  end
end
