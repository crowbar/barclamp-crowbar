# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20150605130326) do

  create_table "proposal_versions", force: :cascade do |t|
    t.integer  "proposal_id", null: false
    t.string   "event",       null: false
    t.string   "barclamp",    null: false
    t.string   "name",        null: false
    t.text     "properties"
    t.datetime "created_at"
  end

  add_index "proposal_versions", ["proposal_id"], name: "index_proposal_versions_on_proposal_id"

  create_table "proposals", force: :cascade do |t|
    t.string "barclamp",   null: false
    t.string "name",       null: false
    t.text   "properties"
  end

  add_index "proposals", ["barclamp", "name"], name: "index_proposals_on_barclamp_and_name", unique: true

  create_table "sessions", force: :cascade do |t|
    t.string   "session_id", limit: 255, null: false
    t.text     "data"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "sessions", ["session_id"], name: "sessions_on_session_id"
  add_index "sessions", ["updated_at"], name: "sessions_on_updated_at"

end
