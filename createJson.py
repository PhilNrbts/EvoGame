import os
import json

# Define the base path to your books
base_path = "/sciencebooks/informaticsbooks/"

# Initialize an empty list to store your documents
documents = []

# Open your text file
with open("your_text_file.txt", 'r') as file:
    lines = file.readlines()
    for i, line in enumerate(lines):
        # Remove trailing newlines
        line = line.rstrip()
        
        # Construct the full path
        full_path = base_path + line
        
        # Get the size of the file in bytes
        original_size = os.path.getsize(full_path)
        
        # Get the format of the file from the extension
        original_format = full_path.split('.')[-1]
        
        # Create the document record
        document = {
            "id": i + 1,  # Start IDs at 1
            "discipline": "informatics",
            "path/title": full_path,
            "original size": original_size,
            "original format": original_format,
            # Other fields here...
        }
        
        # Add the document to your list
        documents.append(document)

# Save your documents to a JSON file
with open("documents.json", 'w') as file:
    json.dump(documents, file, indent=4)

  -----------------------------------------------
  
  
  pip install transformers torch

  from transformers import GPT2LMHeadModel, GPT2Tokenizer

# Initialize the tokenizer and model
tokenizer = GPT2Tokenizer.from_pretrained("gpt2")
model = GPT2LMHeadModel.from_pretrained("gpt2")

# Create some example input
input_text = "This is an example input to GPT-2."

# Tokenize the input
input_ids = tokenizer.encode(input_text, return_tensors="pt")

# Generate output
output = model.generate(input_ids, max_length=100, temperature=0.7, num_return_sequences=1)

# Decode the output
output_text = tokenizer.decode(output[0], skip_special_tokens=True)

print(output_text)
