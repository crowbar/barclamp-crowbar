class Array
  # Splits or iterates over the array in groups of size +number+,
  # padding any remaining slots with +fill_with+ unless it is +false+.
  # Backported from Rails3 to get it into Rails2 too.
  def in_groups_of(number, fill_with = nil)
    if fill_with == false
      collection = self
    else
      padding = (number - size % number) % number
      collection = dup.concat(Array.new(padding, fill_with))
    end

    if block_given?
      collection.each_slice(number) do |slice|
        yield(slice)
      end
    else
      collection.each_slice(number).to_a
    end
  end
end
