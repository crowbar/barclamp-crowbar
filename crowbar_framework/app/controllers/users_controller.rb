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
#
class UsersController < BarclampController
  respond_to :html, :json
  
  helper_method :is_edit_mode?
  
  def initialize #act like a barclamp
    @bc_name = "user"
    @barclamp_object = Barclamp.new
    @barclamp_object.name = "user"
    super
  end
  
  def index
    setup_users
  end

  def unlock_user
    user = User.find(params[:id])
    user.unlock_access! if (user.access_locked?)
    redirect_to users_path, :notice => t("users.index.unlocked")
  end

  def lock_user
    user = User.find(params[:id])
    user.lock_access! if !(user.access_locked?)
    redirect_to users_path, :notice => t("users.index.locked")
  end

  def reset_password
    if !params[:cancel].nil?
      setup_users
      return render :action => :index
    end
    check_password
    
    @user = User.find(params[:user][:id])
    @user.admin_reset_password = true
    if @user.reset_password!(params[:user][:password],params[:user][:password_confirmation])
      redirect_to users_path, :notice => t("users.index.reset_password_success")
    else
      setup_users
      render :action => :index
    end
  end

  def edit
    @user = User.find(params[:id])
    edit_common
  end

  def edit_password
    @user = User.find(params[:id])
    @user.admin_reset_password = true
    edit_common
  end

  def update
    # check_password
    if !params[:cancel].nil?
      setup_users
      return render :action => :index
    end
    
    @user = User.find(params[:id])
    if @user.update_attributes(params[:user])
      redirect_to users_path, :notice => t("users.index.update_success")
    else
      setup_users
      render :action => :index
    end
  end

  def create
    check_password
    
    @user = User.new(params[:user])
    if @user.save
      redirect_to users_path, :notice => t("users.index.create_success")
    else
      setup_users
      render :action => :index
    end
  end

  def delete_users
    if (params['users_to_delete'])
      User.destroy(params['users_to_delete'])
      notice = t("users.index.delete_success")
    else
      notice = t("users.index.none_selected")
    end
    redirect_to users_path, :notice => notice
  end
  
  def is_edit_mode?
    current_user.is_admin? && Rails.env.development?
  end
  
  ############################## API Calls ####################################
  
  add_help(:user_list,[],[:get])
  def users
    Rails.logger.debug("Listing users");

    user_hash = Hash.new
    
    User.all.each { |user|
     user_hash[user.id] = user.username
    }

    respond_to do |format|
      format.json { render :json => user_hash.to_json }
    end
  end

  add_help(:user_show,[:id],[:get])
  def user_show
    id = params[:id]

    Rails.logger.debug("Showing user #{id}");

    ret = nil
    user = nil
    begin
      user = User.find_by_id_or_username(id)
      ret = [200,  user]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      puts "RuntimeError #{ex}"
      ret = [500, ex.message]
    end
    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_create,[:username, :email, :password, :password_confirmation, :remember_me, :is_admin],[:post])
  def user_create
    username = params[:username]
    email = params[:email]
    password = params[:password]
    password_confirmation = params[:password_confirmation]
    remember_me = params[:remember_me]
    is_admin = params[:is_admin]
    Rails.logger.debug("Entering user_create, input:  #{username}, #{email}, #{password}, #{password_confirmation}, #{remember_me}, #{is_admin}")
    ret = nil
    user = nil
    begin
      User.transaction do
        user = User.new(
            :username => username,
            :email => email,
            :password => password,
            :password_confirmation => password_confirmation,
            :remember_me => remember_me,
            :is_admin => is_admin)
        user.save!
      end
      ret = [200, user] 
    rescue ActiveRecord::RecordNotFound, ActiveRecord::RecordInvalid, ArgumentError => ex
      Rails.logger.warn(ex.message)
      ret = [400, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end
    
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    respond_to do |format|
      format.json { render :json => ret[1].to_json() }
    end
  end

  add_help(:user_update,[:id, :username, :email,:remember_me, :is_admin],[:put])
  def user_update
    id = params[:id]
    username = params[:username]
    email = params[:email]
    remember_me = params[:remember_me]
    is_admin = params[:is_admin]

    Rails.logger.debug("Updating user #{id}");

    ret = nil
    user = nil
    begin
      User.transaction do
        user = User.find_by_id_or_username(id)
        hash = {:username => username, :email => email, :remember_me => remember_me, :is_admin => is_admin }
        Rails.logger.debug("Attribute hash is: #{hash.inspect}")
        user.update_attributes(hash)
        user.save!
      end
      ret = [200, user]
    rescue ActiveRecord::RecordNotFound, ArgumentError => ex
      Rails.logger.warn(ex.message)
      ret = [400, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end
    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json() }
    end
  end

  add_help(:user_delete,[:id],[:delete])
   def user_delete
    Rails.logger.debug("Deleting user #{params[:id]}");

    ret = nil
    id = params[:id]
    begin
      user =  User.find_by_id_or_username(id)
      Rails.logger.debug("Deleting user #{user.id}/\"#{user.username}\"")
      User.destroy(user)
      ret = [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:user_make_admin,[:id],[:post])
  def user_make_admin
    id = params[:id]

    Rails.logger.debug("Making user #{id} admin");

    ret = user_update_admin(id, true)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_remove_admin,[:id],[:delete])
  def user_remove_admin
    id = params[:id]

    Rails.logger.debug("Removing admin privilege for user #{id}");

    ret = user_update_admin(id, false)

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_lock,[:id],[:post])
  def user_lock
    id = params[:id]

    Rails.logger.debug("Locking user #{id}");

    ret = nil
    begin
      user = User.find_by_id_or_username(id)
      user.lock_access! unless (user.access_locked?)
      user.save
      ret = [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_unlock,[:id],[:delete])
  def user_unlock
    id = params[:id]

    Rails.logger.debug("Unlocking user #{id}");

    ret = nil
    begin
      user = User.find_by_id_or_username(id)
      user.unlock_access! if (user.access_locked?)
      user.save
      ret = [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
  end

  add_help(:user_reset_password,[:id, :password, :password_confirmation],[:put])
   def user_reset_password
    id = params[:id]
    password = params[:password]
    password_confirmation = params[:password_confirmation]

    Rails.logger.debug("Reset password for user #{id}");

    ret = nil
    begin
      user = User.find_by_id_or_username(id)
      user.admin_reset_password = true
      ret = user.reset_password!(password,password_confirmation)
      if ret
        ret = [200, user]
      else
        raise RuntimeError, "Failed resetting password: #{password}, password confirmation: #{password_confirmation}"
      end
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      ret = [500, ex.message]
    end

    return render :text => ret[1], :status => ret[0] if ret[0] != 200

    respond_to do |format|
      format.json { render :json => ret[1].to_json()}
    end
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
      @users = User.all
      @user ||= User.new
    else
      @users = [current_user]
    end
    
  end
  
  def user_update_admin(id_username, onOff=false)
    begin
      user = User.find_by_id_or_username(id_username)
      user.is_admin = onOff;
      user.save
      [200, user]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      [500, ex.message]
    end
  end
  
end