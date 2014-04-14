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

Crowbar::Application.routes.draw do
  root to: "dashboard#index"

  Dir.glob(File.join(File.dirname(__FILE__), "routes.d", "*.routes")) do |routes_file|
    eval(IO.read(routes_file), binding)
  end

  match "crowbar/modules/1.0", controller: :barclamp, action: :modules, as: :available_barclamps, via: [:get, :post]

  match "docs/:controller/1.0", action: :docs, as: :docs_barclamp, via: [:get, :post]
  match "nodes/:controller/1.0", action: :nodes, as: :nodes_barclamp, via: [:get, :post]
  match "utils/:controller/1.0", action: :utils, as: :utils_barclamp, via: [:get, :post]
  match "network/:controller/1.0", action: :network, as: :network_barclamp, via: [:get, :post]
  match "export/:controller/1.0", action: :export, as: :export_barclamp, via: [:get, :post]

  resources :translations, only: [:index]

  constraints(id: /[^\/]+/) do
    get "dashboard" => "dashboard#index", as: :dashboard

    resources :utils, only: [:index, :destroy] do
      collection do
        get ":file" => "utils#index", as: :file, constraints: { file: /[^\/]+/}
      end
    end

    resources :docs, only: [:index] do
      collection do
        get "*id" => "docs#show", as: :topic
      end
    end

    resources :nodes, only: [:edit, :update, :show] do
      collection do
        get :status
        get :unallocated
        get :list
        get :families
        post :bulk
      end

      member do
        get :status
        post :group

        get "hit/:req" => "nodes#hit", as: :hit
        get "attribute/*path" => "nodes#attribute", as: :attribute, constraints: { path: /.*/ }
      end
    end

    resources :networks, only: [], controller: :network do
      member do 
        get :switch
        get :vlan
      end
    end
  end

  scope "crowbar/:controller/1.0" do
    match "help", action: :help, as: :help_barclamp, via: [:get]

    match "status", action: :state, as: :status_barclamp, via: [:get]
    match "transition/:id", action: :transition, as: :transition_barclamp, via: [:get, :post]

    match "proposals", action: :proposal_index, as: :proposals, via: [:get]
    match "proposals", action: :proposal_create, as: :create_proposal, via: [:put]
    match "proposals/:id", action: :proposal_update, as: :update_proposal, via: [:post]
    match "proposals/:id", action: :proposal_edit, as: :edit_proposal, via: [:get]
    match "proposals/status/:id", action: :proposal_status, as: :status_proposal, via: [:get]
    match "proposals/show/:id", action: :proposal_show, as: :show_proposal, via: [:get]
    match "proposals/deactivate/:id", action: :proposal_deactivate, as: :deactivate_proposal, via: [:get]
    match "proposals/delete/:id", action: :proposal_delete, as: :delete_proposal, via: [:get]
    match "proposals/dequeue/:id", action: :proposal_dequeue, as: :dequeue_proposal, via: [:get]
    match "proposals/commit/:id", action: :proposal_commit, as: :commit_proposal, via: [:get]

    match "elements", action: :element_index, as: :elements, via: [:get]
    match "elements/:id", action: :element_show, as: :show_element, via: [:get]
  end

  constraints(id: /[^\/]+/) do
    match "crowbar/:controller/1.0", action: :index, as: :grouped_barclamps, via: [:get]
    match "crowbar/:controller/1.0/:action/:id", as: :on_barclamp, via: [:post]
  end

  get "crowbar/:controller", action: :versions

  scope "crowbar/:barclamp/1.0", controller: "barclamp" do
    match "help", action: :help, via: [:get]

    match "status", action: :state, via: [:get]
    match "transition/:id", action: :transition, via: [:get, :post]

    match "proposals", action: :proposal_index, via: [:get]
    match "proposals", action: :proposal_create, via: [:put]
    match "proposals/:id", action: :proposal_update, via: [:post]
    match "proposals/:id", action: :proposal_edit, via: [:get]
    match "proposals/status/:id", action: :proposal_status, via: [:get]
    match "proposals/show/:id", action: :proposal_show, via: [:get]
    match "proposals/deactivate/:id", action: :proposal_deactivate, via: [:get]
    match "proposals/delete/:id", action: :proposal_delete, via: [:get]
    match "proposals/dequeue/:id", action: :proposal_dequeue, via: [:get]
    match "proposals/commit/:id", action: :proposal_commit, via: [:post]

    match "elements", action: :element_index, via: [:get]
    match "elements/:id", action: :element_show, via: [:get]
  end

  constraints(id: /[^\/]+/ ) do
    match "crowbar/:barclamp/1.0", controller: :barclamp, action: :index, via: [:get]
    match "crowbar/:barclamp/1.0/:action/:id", controller: :barclamp, via: [:post]
  end

  get "crowbar/:barclamp", controller: :barclamp, action: :versions
end
