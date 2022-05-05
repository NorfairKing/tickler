CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"username" VARCHAR NOT NULL,"hashed_password" BLOB NOT NULL,"created" TIMESTAMP NOT NULL,"last_login" TIMESTAMP NULL,CONSTRAINT "unique_user_identifier" UNIQUE ("identifier"),CONSTRAINT "unique_username" UNIQUE ("username"));
CREATE TABLE "customer"("id" INTEGER PRIMARY KEY,"user" BLOB NOT NULL,"stripe_customer" VARCHAR NOT NULL,CONSTRAINT "unique_customer_user" UNIQUE ("user"),CONSTRAINT "unique_user_customer" UNIQUE ("stripe_customer"));
CREATE TABLE "stripe_event"("id" INTEGER PRIMARY KEY,"event" VARCHAR NOT NULL,"error" VARCHAR NULL,CONSTRAINT "unique_stripe_event" UNIQUE ("event"));
CREATE TABLE "user_settings"("id" INTEGER PRIMARY KEY,"user_id" BLOB NOT NULL,"time_zone" VARCHAR NOT NULL,CONSTRAINT "unique_user_settings" UNIQUE ("user_id"));
CREATE TABLE "tickler_item"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"user_id" BLOB NOT NULL,"contents" VARCHAR NOT NULL,"created" TIMESTAMP NOT NULL,"scheduled_day" DATE NOT NULL,"scheduled_time" TIME NULL,"recurrence" VARCHAR NULL,CONSTRAINT "unique_item_identifier" UNIQUE ("user_id","identifier"));
CREATE TABLE "triggered_item"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"user_id" BLOB NOT NULL,"contents" VARCHAR NOT NULL,"created" TIMESTAMP NOT NULL,"scheduled_day" DATE NOT NULL,"scheduled_time" TIME NULL,"recurrence" VARCHAR NULL,"triggered" TIMESTAMP NOT NULL,CONSTRAINT "unique_triggered_item_identifier" UNIQUE ("identifier"));
CREATE TABLE "user_trigger"("id" INTEGER PRIMARY KEY,"user_id" BLOB NOT NULL,"trigger_type" VARCHAR NOT NULL,"trigger_id" BLOB NOT NULL);
CREATE TABLE "intray_trigger"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"url" VARCHAR NOT NULL,"username" VARCHAR NOT NULL,"access_key" BLOB NOT NULL,"added" TIMESTAMP NOT NULL,CONSTRAINT "unique_intray_trigger" UNIQUE ("identifier"));
CREATE TABLE "email_trigger"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"address" VARCHAR NOT NULL,"verification_key" BLOB NOT NULL,"verified" BOOLEAN NOT NULL,"added" TIMESTAMP NOT NULL,CONSTRAINT "unique_email_trigger" UNIQUE ("identifier"));
CREATE TABLE "verification_email"("id" INTEGER PRIMARY KEY,"to" VARCHAR NOT NULL,"key" BLOB NOT NULL,"trigger" BLOB NOT NULL,"scheduled" TIMESTAMP NOT NULL,"email" INTEGER NULL REFERENCES "email");
CREATE TABLE "admin_notification_email"("id" INTEGER PRIMARY KEY,"contents" VARCHAR NOT NULL,"email" INTEGER NULL REFERENCES "email");
CREATE TABLE "triggered_intray_item"("id" INTEGER PRIMARY KEY,"item" BLOB NOT NULL,"trigger" BLOB NOT NULL,"intray_item_u_u_i_d" BLOB NULL,"error" VARCHAR NULL,CONSTRAINT "unique_triggered_intray_item" UNIQUE ("item","trigger"));
CREATE TABLE "triggered_email"("id" INTEGER PRIMARY KEY,"item" BLOB NOT NULL,"trigger" BLOB NOT NULL,"email" INTEGER NULL REFERENCES "email","error" VARCHAR NULL,CONSTRAINT "unique_triggered_email" UNIQUE ("item","trigger"));
CREATE TABLE "email"("id" INTEGER PRIMARY KEY,"to" VARCHAR NOT NULL,"from" VARCHAR NOT NULL,"from_name" VARCHAR NOT NULL,"subject" VARCHAR NOT NULL,"text_content" VARCHAR NOT NULL,"html_content" VARCHAR NOT NULL,"status" VARCHAR NOT NULL,"send_error" VARCHAR NULL,"ses_id" VARCHAR NULL,"scheduled" TIMESTAMP NOT NULL,"send_attempt" TIMESTAMP NULL);

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Tickler.Data.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.
