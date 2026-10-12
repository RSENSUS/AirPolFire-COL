# 01_hysplit_backtrajectories.R
# Daily 120-h HYSPLIT back-trajectories arriving over Medellín (Aburrá Valley)

# Output: FlagHysplit<year>.txt  (one row per trajectory point; columns:
#         Fecha, Hora, Latitud, Longitud, Altura, Presion, Flag)
# Input:  GDAS 1-degree meteorology, downloaded automatically by splitr
# Requires: HYSPLIT installed, R packages splitr, dplyr, lubridate, here

#Load packages
{
  library(splitr)
  library(here)
  library(lubridate)
  library(dplyr)
}

# Configure route with guaranteed permissions
out_path <- file.path(Sys.getenv("USERPROFILE"), "hysplit_output")
met_path <- file.path(Sys.getenv("USERPROFILE"), "met_data")

# Make directories
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
dir.create(met_path, recursive = TRUE, showWarnings = FALSE)

# Create SETUP.CFG
ruta_setup <- file.path(out_path, "SETUP.CFG")
writeLines(
  c(
    "&SETUP",
    "tratio = 0.75,", 
    "delt = 0.0,", 
    "kmix0 = 0,",  # Critic parameter
    "/"
  ), 
  con = ruta_setup
)

#LOOP THE TRAJECTORY FUNCTION FOR ONE YEAR

# Define year
year_to_run <- 2019  # Change the year if needed

# Initialize an empty list to store the results
#trajectory_results <- list()

# Create a loop for each month
for (month in 1:12) {
  
  # Define start and end dates for the month
  start_date <- as.Date(paste0(year_to_run, "-", sprintf("%02d", month), "-01"))
  end_date <- as.Date(paste0(year_to_run, "-", sprintf("%02d", month), "-", days_in_month(start_date)))
  
  # Checkpoint: Print current month being processed
  cat("\nRunning HYSPLIT for:", month.name[month], year_to_run, "\n")
  cat("Processing dates from", start_date, "to", end_date, "\n")
  
  # Run the HYSPLIT trajectory model for the current month
  trajectory_data <- tryCatch(
    {
      hysplit_trajectory(
        lat = 6.243,
        lon = -75.612,
        direction = "backward",
        height = 1500,
        duration = 120,
        days = seq(ymd(start_date), ymd(end_date), by = "1 day"),
        daily_hours = 11,  # UTC hour
        met_type = "gdas1",
        extended_met = TRUE,
        met_dir = met_path,
        exec_dir = out_path
      )
    },
    error = function(e) {
      cat("Error encountered in", month.name[month], "on", start_date, "to", end_date, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL)  # Return NULL to avoid script breaking
    }
  )
  
  # If trajectory_data is NULL, skip saving
  if (!is.null(trajectory_data)) {
    # Assign the result to a dynamically named variable (e.g., traj1, traj2, etc.)
    assign(paste0("traj", month), trajectory_data, envir = .GlobalEnv)
    
    # Also store it in a list for easy access
    #trajectory_results[[month]] <- trajectory_data
    
    cat("Saved trajectory for:", month.name[month], "\n")
  } else {
    cat("Skipping", month.name[month], "due to error.\n")
  }
}

# List of trajectory datasets
trajectory_list <- list(traj1, traj2, traj3, traj4, traj5, traj6, traj7, traj8, traj9, traj10, traj11, traj12)

# Combine all months into one dataframe
trajectory_data <- bind_rows(trajectory_list)

# Ensure traj_dt_i (start date of trajectory) is properly formatted
trajectory_data$traj_dt_i <- as.Date(trajectory_data$traj_dt_i)

# Sort by start date of the trajectory
trajectory_data <- trajectory_data %>%
  arrange(traj_dt_i, run)

# Assign a unique sequential Flag (1 to 365) based on traj_dt_i
trajectory_data <- trajectory_data %>%
  mutate(Flag = dense_rank(traj_dt_i))

# Verify Flag sequence
unique(trajectory_data$Flag)

# Modify trajectory_data to add 'Hora' column and convert 'traj_dt' to date-only format
trajectory_data <- trajectory_data %>%
  mutate(
    Hora = hour(traj_dt),          # Extract hour from traj_dt
    traj_dt = as.Date(traj_dt)     # Convert to Y-m-d format
  )

# Select only the required columns in the specified order
trajectory_data <- trajectory_data %>%
  select(traj_dt, Hora, lat, lon, height, pressure, Flag)

#Modify names
names(trajectory_data) <- c('Fecha','Hora',	'Latitud',	'Longitud',	'Altura',	'Presion', 	'Flag')

# Save the merged file with the new Flag column
save_path <- 'C:/Users/tur75984/OneDrive - Temple University/ResearchAssis/CODI/R-CODI/PointIntersections/Traj1500'
write.table(trajectory_data, paste0(save_path,"/FlagHysplit",year_to_run,".txt"), row.names = FALSE)

