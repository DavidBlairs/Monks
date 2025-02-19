### **Monks Submission**  

This is an R Shiny dashboard designed to visualise statistical modelling data. It provides interactive plots and a data table, allowing users to explore model results efficiently.  

<img width="1673" alt="image" src="https://github.com/user-attachments/assets/b8f6e4f5-6b12-484d-b977-5ecdd28e3f1c" />

## **Features**  
- **Three interactive ggplot visualisations**, each with its own filtering options.  
- **Data table (DT) with filters**, including a **calculated ROI column**.  
- **Natural language filtering**: Users can ask a question, and the dashboard will update filters automatically (powered by `ellmer` and OpenAI’s GPT-4o-mini).  
- **Database connectivity**: Retrieves data from an **Azure SQL database**.  

## **Live Demo**  
The application is hosted at:  
🔗 [Monk Submission Dashboard](https://unileverdemo.shinyapps.io/monk/) 

You might need to refresh the page a couple of times to allow the SQL server instance to start up. 

## **Running Locally**  

This application has been tested on an M1 Macbook Air with MacOS Sequoia. We cannot garantee it will work on other hardware / operating systems. 

### **Setup & Installation**  
1. Clone this repository  
2. Restore dependences:  
   ```r
   renv::restore()
   ```  
3. Set up environment variables:  
   - `OPENAI_API_KEY` → OpenAI API key  
   - `DB_UID` → Database username  
   - `DB_PWD` → Database password  
4. Set `is_local=T` in `main.R:18`.
5. Run the app:  
   ```r
   rhino::app()
   ```  
