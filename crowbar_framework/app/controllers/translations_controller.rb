# -*- encoding : utf-8 -*-
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class TranslationsController < ApplicationController
  def index
    respond_to do |format|
      format.json { render json: translations }
    end
  end

  protected

  def translations
    I18n.backend.load_translations

    values = I18n.backend.send(:translations)
    locale = params[:lang] || I18n.locale

    flattening(
      values[locale.to_sym]
    )
  end

  def flattening(values, keys = [])
    {}.tap do |result|
      values.each do |key, value|
        path = keys.dup.push(key)

        if value.is_a? Hash
          result.merge! flattening(value, path)
        else

          result[path.join(".")] = if value.is_a? Array
            value.compact.map { |v| v.gsub(/%{(.*)}/, "{{\\1}}") }
          else
            value.to_s.gsub(/%{(.*)}/, "{{\\1}}")
          end
        end
      end
    end
  end
end
