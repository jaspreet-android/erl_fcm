create DATABASE fcm;
CREATE TABLE fcm_tokens (
    id SERIAL PRIMARY KEY,
    user_id UUID NOT NULL,
    fcm_token TEXT NOT NULL,
    device_type VARCHAR(50),
    token TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create an index on user_id for faster lookups
CREATE INDEX idx_user_id ON fcm_tokens(user_id);

-- Trigger to update the updated_at field on row update
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
   NEW.updated_at = NOW();
   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_updated_at_trigger
BEFORE UPDATE ON fcm_tokens
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at_column();