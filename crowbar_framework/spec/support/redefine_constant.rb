def with_constant(const_sym, val, scope = Object)
  old_val   = scope.const_get(const_sym)

  silence_warnings { scope.const_set(const_sym, val) }
  yield
ensure
  silence_warnings { scope.const_set(const_sym, old_val) }
end

