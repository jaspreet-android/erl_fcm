ALTER TABLE fcm_tokens
ADD COLUMN luser VARCHAR(100) DEFAULT NULL;

-- Add the 'resource' column to the table if it does not exist
ALTER TABLE fcm_tokens
ADD COLUMN resource VARCHAR(20);

-- Drop the existing primary key constraint if one exists
ALTER TABLE fcm_tokens
DROP CONSTRAINT IF EXISTS fcm_tokens_pkey;

-- Add the composite primary key constraint
ALTER TABLE fcm_tokens
ADD CONSTRAINT fcm_tokens_pkey PRIMARY KEY (luser);


ALTER TABLE fcm_tokens
ALTER COLUMN fcm_token SET NOT NULL,
ALTER COLUMN luser SET NOT NULL,
ALTER COLUMN resource SET NOT NULL;
