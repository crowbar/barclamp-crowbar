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

class DashboardController < ApplicationController
  def index
    @nodes = {}.tap do |nodes|
      @groups = {}.tap do |groups|
        NodeObject.all.each do |node|
          nodes[node.handle] = node.to_h

          groups[node.group] ||= {
            handle: node.group,
            title: view_context.truncate(node.group || t("unknown"), length: 25),
            automatic: !node.display_set?("group"),
            nodes: {},
            status: {
              ready: 0,
              failed: 0,
              unknown: 0,
              unready: 0,
              pending: 0
            }
          }

          groups[node.group][:nodes][node.handle] = node.to_h
          groups[node.group][:status][node.status.to_sym] += 1
        end





        # groups.each do |name, attrs|
        #   values = view_context.piechart_values(attrs)
        #   tooltip = view_context.piechart_tooltip(values)

        #   attrs[:chart] = {
        #     values: values,
        #     tooltip: tooltip
        #   }
        # end





      end
    end

    if @nodes.empty?
      flash.now[:warning] = I18n.t("dashboard.errors.no_nodes")
    end

    respond_to do |format|
      format.html
      format.xml { render :xml => @groups }
      format.json { render :json => @groups }
    end
  end
end
