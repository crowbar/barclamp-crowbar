# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
class UsersController < ApplicationController
  respond_to :html, :json
  
  helper_method :is_edit_mode?

  add_help(:index,[],[:get])
  def index
    @users = User.all
    respond_to do |format|
      format.html { }
      format.json { render api_index :user, @users }
    end
  end
  
 # RESTful DELETE of the node resource
  def destroy
    @user = User.find_key params[:id]
    @user ||= User.find_by_username params[:id]
    render api_delete User, @user.id
  end
  
  add_help(:create,[:username, :email, :password, :password_confirmation, :remember_me, :is_admin],[:post])
  def create
    u = User.create! params
    render api_show :user, User, u.id.to_s, nil, u
  end
  
  def update
    @user = User.find_key params[:id]
    @user ||= User.find_by_username params[:id]
    render api_update :user, User, @user
  end

  add_help(:show,[:id],[:get])
  def show
    @user = User.find_key params[:id]
    @user ||= User.find_by_username params[:id]
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show :user, User, @user }
    end
  end

  add_help(:unlock,[:id],[:delete]) 
  def unlock
    # ZEHICLE TODO REFACTOR!
    ret = fetch_user
    respond_with(@user)  do |format|
      @user.unlock_access! if (!@user.nil? and @user.access_locked?)
      format.html do
        redirect_to users_path, :notice => t("users.index.unlocked")
      end
      format.json do
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render :json => @user.to_json
      end
    end
  end

  add_help(:lock,[:id],[:post])
  def lock
    # ZEHICLE TODO REFACTOR!
    ret = fetch_user
    respond_with(@user)  do |format|
      @user.lock_access! if (!@user.nil? and !@user.access_locked?)
      format.html do
        redirect_to users_path, :notice => t("users.index.locked")
      end
      format.json do
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render :json => @user.to_json
      end
   end
 end 
 
 add_help(:reset_password,[:id, :password, :password_confirmation],[:put])
 def reset_password
    # ZEHICLE TODO REFACTOR!
   ret = fetch_user
  respond_with(@user)  do |format|
    Rails.logger.debug("Reset password for user #{@user}")
    format.html do
      if !params[:cancel].nil?
        @user = nil
        setup_users
        return render :action => :index
      end
      check_password
      @user.admin_reset_password = true
      if @user.reset_password!(params[:user][:password],params[:user][:password_confirmation])
        redirect_to users_path, :notice => t("users.index.reset_password_success")
      else
        setup_users
        render :action => :index
      end
    end
    format.json do
      password = params[:password]
      password_confirmation = params[:password_confirmation]
      begin
         @user.admin_reset_password = true
         reset_success = @user.reset_password!(password, password_confirmation)
         raise ActiveRecord::RecordInvalid.new(@user) unless reset_success
      rescue ActiveRecord::RecordInvalid, ArgumentError => ex
          Rails.logger.error(ex.message)
          ret = [500, ex.message]
      end  if ret[0]==200
      return render :text => ret[1], :status => ret[0] unless ret[0] == 200
      render :json => @user.to_json
    end
   end
  end

  def is_edit_mode?
    current_user.is_admin? && Rails.env.development?
  end
  
  add_help(:make_admin,[:id],[:post])
  def make_admin
    ret = fetch_user
    respond_with(@user)  do |format|
      Rails.logger.debug("Making user #{@user.id} admin") unless @user.nil?
      format.html do
        @user.is_admin = true;
        @user.save
        render
      end
      format.json do
        ret = update_admin(true) if ret[0] == 200
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render :json => @user.to_json
      end
    end
  end

  add_help(:remove_admin,[:id],[:delete])
  def remove_admin
    ret = fetch_user
    respond_with(@user) do |format|
      format.html do
        @user.is_admin = false;
        @user.save
        render
      end
      format.json do
        ret = update_admin(false) if ret[0] == 200
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render :json => @user.to_json
      end
    end
  end
  
  def edit
    fetch_user
    edit_common
  end

  def edit_password
    code, exception = fetch_user
    @user.admin_reset_password = true if code == 200
    edit_common
  end

  private
  
  def edit_common
    setup_users
    render :action => :index
  end

  def check_password
    if params[:user][:password].blank?
      params[:user].delete(:password)
      params[:user].delete(:password_confirmation)
    end
  end

  def setup_users
    if (current_user.is_admin)
      @users = User.all if @users.nil?
      @user ||= User.new
    else
      @users = [current_user]
    end
    
  end

  def update_admin(onOff=false)
    begin
      @user.is_admin = onOff;
      @user.save
      [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      [500, ex.message]
    end
  end
  
 def fetch_user
    ret = nil
    begin
      @user = User.find_by_id_or_username((params[:user].nil? or params[:user][:id].nil?) ? \
      ((params[:user_id].nil?) ? params[:id] : params[:user_id]) : \
      params[:user][:id])
      ret = [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      puts "RuntimeError #{ex}"
      ret = [500, ex.message]
    end
    ret
  end
  
  def fetch_users
    ret = nil
    begin
      @users = User.all
      ret = [200,  ""]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      puts "RuntimeError #{ex}"
      ret = [500, ex.message]
    end
    ret
  end
  
  def populate_user
    return User.new(params[:user]) unless params[:user].nil?
    User.new(:username => params[:username], :email => params[:email], :password => params[:password], \
     :password_confirmation => params[:password_confirmation], :remember_me => params[:remember_me], \
     :is_admin => params[:is_admin])
  end
end
