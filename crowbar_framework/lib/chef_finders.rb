module ChefFinders
  class RecordNotFound < StandardError; end

  def chef_class
    Class
  end

  def chef_type
    "class"
  end

  def after_find_filter(results)
    results
  end

  def method_missing(m, *a, &block)
    return super unless m.to_s =~ /^find_(all_)?by_(.*?)(!)?$/

    find_or_fail = !$3.blank?
    find_first   =  $1.blank?
    params       =  $2.split("_and_").map(&:to_sym)

    unless params.count == a.count
      raise ArgumentError.new("Wrong number of arguments (#{a.count} for #{params.count})")
    end

    args       = a.slice(0, params.count)
    conditions = Hash[params.zip(args)]
    result     = where(conditions)

    if find_first && result
      result = result.first
    end

    if find_or_fail
      if (find_first && result.nil?) || (!find_first && result.empty?)
        raise RecordNotFound.new("Cannot find any records matching query #{build_query(conditions)}")
      end
    end

    result
  end

  def load(id)
    begin
      return new(chef_class.load(id))
    rescue Net::HTTPServerException => e
      if e.response.code == "404"
        Rails.logger.warn("[chef search] #{chef_type} #{id} not found.")
        nil
      else
        raise e
      end
    end
  end

  def all
    where
  end

  def where(conditions = {})
    results, offset, count = raw_search(conditions)
    results.map! { |r| new(r) }
    after_find_filter(results)
  end

  def chef_escape(str)
    str.to_s.gsub("-:") { |c| '\\' + c }
  end

  def query_object
    begin
      Chef::Search::Query.new
    rescue
      Chef::Node.new
    end
  end

  def raw_search(conditions = {})
    query = build_query(conditions)
    query_object.search(chef_type, query)
  end

  def build_query(conditions = {}, op = :and)
    return chef_escape("*:*") if conditions.empty?

    conditions.map do |k, v|
      if v.is_a?(Array)
        v.map { |x| "#{k}:#{chef_escape(x)}" }.join(" #{op} ")
      elsif v.is_a?(Hash)
        "(" + build_query(v, k == :or ? :or : :and) + ")"
      else
        "#{k}:#{chef_escape(v)}"
      end
    end.join(" #{op} ")
  end
end
