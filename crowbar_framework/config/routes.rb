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
  root :to => "dashboard#index"

  Dir.glob(File.join(File.dirname(__FILE__), "routes.d", "*.routes")) do |routes_file|
    eval(IO.read(routes_file), binding)
  end

  constraints(id: /.*/ ) do
    get "dashboard" => "dashboard#index", as: :dashboard
    get "dashboard/:id" => "dashboard#show", as: :dashboard_detail

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

    resources :utils, only: [:index, :destroy], controller: :support do
      collection do
        get :chef # maybe move into custom export controller? export_barclamp_path(controller: "chef")
        get :supportconfig # maybe move into custom export controller? export_barclamp_path(controller: "supportconfig")
      end

      member do
        get :restart
        post :import
        post :upload
      end
    end

    resources :networks, only: [] do
      member do 
        get :switch
        get :vlan
      end
    end

    resources :docs, only: [:index] do
      collection do
        get "*id" => "docs#show"
      end
    end
  end

  match "crowbar/modules/1.0", controller: "barclamp", action: "modules", as: :crowbar_barclamps, via: [:get, :post]

  match "nodes/:controller/1.0", action: "nodes", as: :nodes_barclamp, via: [:get, :post]
  match "utils/:controller/1.0", action: "utils", as: :utils_barclamp, via: [:get, :post]
  match "export/:controller/1.0", action: "export", as: :export_barclamp, via: [:get, :post]
  match "network/:controller/1.0", action: "network", as: :network_barclamp, via: [:get, :post]
  match "docs/:controller/1.0", action: "docs", as: :docs_barclamp, via: [:get, :post]

  constraints(id: /.*/ ) do
    get "crowbar/:controller/1.0", action: :index, as: :barclamps
    get "crowbar/:controller/1.0/:id", action: :show, as: :barclamp_show
    delete "crowbar/:controller/1.0/:id", action: :delete, as: :barclamp_delete
    post "crowbar/:controller/1.0/:action/:id", as: :barclamp_action
  end

  scope "crowbar/:controller/1.0" do
    match "help", action: "help", as: :help, via: [:get]

    match "proposals", action: "proposals", as: :proposals, via: [:get]
    match "proposals", action: "proposal_create", as: :proposal_create, via: [:put]
    match "proposals/:id", action: "proposal_update", as: :proposal_update, via: [:post]
    match "proposals/:id", action: "proposal_show", as: :proposal_show, via: [:get]
    match "proposals/delete/:id", action: "proposal_delete", as: :proposal_delete, via: [:get]
    match "proposals/dequeue/:id", action: "proposal_dequeue", as: :proposal_dequeue, via: [:get]
    match "proposals/status/:id", action: "proposal_status", as: :proposal_status, via: [:get]
    match "proposals/commit/:id", action: "proposal_commit", as: :proposal_commit, via: [:post]

    match "elements", action: "elements", as: :elements, via: [:get]
    match "elements/:id", action: "element_info", as: :element, via: [:get]

    match "transition/:id", action: "transition", as: :transition, via: [:get, :post]
  end
  get "crowbar/:controller", action: "versions"

  constraints(id: /.*/ ) do
    get "crowbar/:barclamp/1.0", controller: "barclamp", action: :index
    get "crowbar/:barclamp/1.0/:id", controller: "barclamp", action: :show
    delete "crowbar/:barclamp/1.0/:id", controller: "barclamp", action: :delete
    post "crowbar/:barclamp/1.0/:action/:id", controller: "barclamp"
  end

  scope "crowbar/:barclamp/1.0", controller: "barclamp" do
    match "help", action: "help", via: [:get]

    match "proposals", action: "proposals", via: [:get]
    match "proposals", action: "proposal_create", via: [:put]
    match "proposals/:id", action: "proposal_update", via: [:post]
    match "proposals/:id", action: "proposal_show", via: [:get]
    match "proposals/delete/:id", action: "proposal_delete", via: [:get]
    match "proposals/dequeue/:id", action: "proposal_dequeue", via: [:get]
    match "proposals/status/:id", action: "proposal_status", via: [:get]
    match "proposals/commit/:id", action: "proposal_commit", via: [:post]

    match "elements", action: "elements", via: [:get]
    match "elements/:id", action: "element_info", via: [:get]

    match "transition/:id", action: "transition", via: [:get, :post]
  end
  get "crowbar/:barclamp", controller: "barclamp", action: "versions" 
end
