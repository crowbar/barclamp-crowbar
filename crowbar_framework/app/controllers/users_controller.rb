class UsersController < ApplicationController
  respond_to :html, :xml, :json
  
  def index
    setup_users
  end

  def unlock_user
    user = User.find(params[:id])
    if (user.access_locked?)
      user.unlock_access!
    end
    redirect_to users_path
  end

  def lock_user
    user = User.find(params[:id])
    if !(user.access_locked?)
      user.lock_access!
    end
    redirect_to users_path
  end

  def reset_password
    check_password
    
    @user = User.find(params[:user][:id])
    @user.admin_reset_password = true
    if @user.reset_password!(params[:user][:password],params[:user][:password_confirmation])
      redirect_to users_path, :notice => t("user.reset_password_success")
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

  def edit_common
    setup_users
    render :action => :index
  end

  def update
    check_password

    @user = User.find(params[:id])
    if @user.update_attributes(params[:user])
      redirect_to users_path, :notice => t("user.update_success")
    else
      setup_users
      render :action => :index
    end
  end

  def create
    check_password
    
    @user = User.new(params[:user])
    if @user.save
      #flash[:notice] = t "user.create_success"
      redirect_to users_path, :notice => t("user.create_success")
    else
      setup_users
      render :action => :index
    end
  end

  def delete_users
    if (params['users_to_delete'])
      User.destroy(params['users_to_delete'])
      notice = t("user.delete_success")
    else
      notice = t("user.none_selected")
    end
    redirect_to users_path, :notice => notice
  end

  private

  def check_password
    if params[:user][:password].blank?
      params[:user].delete(:password)
      params[:user].delete(:password_confirmation)
    end
  end

  def setup_users
    @users = User.all
    @user ||= User.new
  end

end