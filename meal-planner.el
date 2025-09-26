;;; meal-planner.el --- Weekly meal planning with rotation tracking

;; Author: Your Name
;; Version: 1.0
;; Keywords: convenience, tools

;;; Commentary:

;; This package provides functions to generate weekly meal suggestions
;; from text files, with rotation tracking to avoid duplicates across weeks.

;;; Code:

(defgroup meal-planner nil
  "Weekly meal planning with rotation tracking."
  :group 'convenience)

(defcustom meal-planner-data-directory "~/.emacs.d/meal-data/"
  "Directory containing meal data files."
  :type 'directory
  :group 'meal-planner)

(defcustom meal-planner-rotation-weeks 4
  "Number of weeks to avoid duplicating meals."
  :type 'integer
  :group 'meal-planner)

(defcustom meal-planner-history-file "meal-history.el"
  "File to store meal selection history."
  :type 'string
  :group 'meal-planner)

;; Data structure to track meal history
(defvar meal-planner-history nil
  "History of selected meals by week and category.")

;; Meal categories and their corresponding files
(defvar meal-planner-categories
  '((weekday-lunch . "weekday-lunch.txt")
    (weekday-sweet . "weekday-sweet.txt")
    (weekday-dinner . "weekday-dinner.txt")
    (weekday-pudding . "weekday-pudding.txt")
    (weekend-lunch . "weekend-lunch.txt")
    (weekend-dinner . "weekend-dinner.txt")
    (weekend-dessert . "weekend-dessert.txt"))
  "Mapping of meal categories to their data files.")

;; Weekly meal structure - batch cooking approach
(defvar meal-planner-week-structure
  '((weekdays . (weekday-lunch weekday-sweet weekday-dinner weekday-pudding))
    (weekend . (weekend-lunch weekend-dinner weekend-dessert)))
  "Structure defining meal categories for weekday batch and weekend batch.")

(defun meal-planner--ensure-data-directory ()
  "Ensure the meal data directory exists."
  (unless (file-exists-p meal-planner-data-directory)
    (make-directory meal-planner-data-directory t)))

(defun meal-planner--get-data-file (category)
  "Get the full path for a meal category data file."
  (expand-file-name 
   (cdr (assq category meal-planner-categories))
   meal-planner-data-directory))

(defun meal-planner--load-meals-from-file (category)
  "Load meals from the specified category file."
  (let ((file (meal-planner--get-data-file category)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t "[ \t\r\n]+")))))

(defun meal-planner--load-history ()
  "Load meal selection history from file."
  (let ((history-file (expand-file-name meal-planner-history-file 
                                       meal-planner-data-directory)))
    (when (file-exists-p history-file)
      (with-temp-buffer
        (insert-file-contents history-file)
        (condition-case nil
            (setq meal-planner-history (read (buffer-string)))
          (error (setq meal-planner-history nil)))))))

(defun meal-planner--save-history ()
  "Save meal selection history to file."
  (meal-planner--ensure-data-directory)
  (let ((history-file (expand-file-name meal-planner-history-file 
                                       meal-planner-data-directory)))
    (with-temp-file history-file
      (prin1 meal-planner-history (current-buffer)))))

(defun meal-planner--get-recent-meals (category weeks-back)
  "Get meals selected for CATEGORY in the last WEEKS-BACK weeks."
  (let ((recent-meals '())
        (current-week (meal-planner--current-week-number)))
    (dotimes (i weeks-back)
      (let* ((week (- current-week i))
             (week-data (assq week meal-planner-history))
             (category-meals (cdr (assq category (cdr week-data)))))
        (when category-meals
          (setq recent-meals (append recent-meals category-meals)))))
    recent-meals))

(defun meal-planner--current-week-number ()
  "Get current week number (simple implementation using days since epoch)."
  (/ (time-to-days (current-time)) 7))

(defun meal-planner--filter-available-meals (category all-meals)
  "Filter out recently used meals from ALL-MEALS for CATEGORY."
  (let ((recent-meals (meal-planner--get-recent-meals 
                      category meal-planner-rotation-weeks)))
    (seq-difference all-meals recent-meals)))

(defun meal-planner--select-random-meal (category)
  "Select a random meal from CATEGORY, avoiding recent duplicates."
  (let* ((all-meals (meal-planner--load-meals-from-file category))
         (available-meals (meal-planner--filter-available-meals category all-meals)))
    (when available-meals
      (nth (random (length available-meals)) available-meals))))

(defun meal-planner--record-meal-selection (week period category meal)
  "Record a meal selection in the history."
  (let* ((week-entry (assq week meal-planner-history))
         (period-entry (assq period (cdr week-entry)))
         (category-meals (cdr (assq category (cdr period-entry)))))
    ;; Update the nested structure
    (unless week-entry
      (push (cons week nil) meal-planner-history)
      (setq week-entry (assq week meal-planner-history)))
    
    (unless period-entry
      (push (cons period nil) (cdr week-entry))
      (setq period-entry (assq period (cdr week-entry))))
    
    (unless (assq category (cdr period-entry))
      (push (cons category nil) (cdr period-entry)))
    
    ;; Add the meal to the category
    (let ((cat-entry (assq category (cdr period-entry))))
      (push meal (cdr cat-entry)))))

(defun meal-planner-generate-week (&optional week-offset)
  "Generate meal suggestions for a week.
WEEK-OFFSET can be used to generate meals for future weeks (default 0)."
  (interactive "P")
  (meal-planner--load-history)
  (let* ((week-num (+ (meal-planner--current-week-number) 
                     (or week-offset 0)))
         (results '()))
    
    ;; Generate meals for weekdays and weekend periods
    (dolist (period-config meal-planner-week-structure)
      (let ((period (car period-config))
            (categories (cdr period-config))
            (period-meals '()))
        
        (dolist (category categories)
          (let ((meal (meal-planner--select-random-meal category)))
            (when meal
              (push (cons category meal) period-meals)
              (meal-planner--record-meal-selection week-num period category meal))))
        
        (push (cons period (reverse period-meals)) results)))
    
    (meal-planner--save-history)
    (meal-planner--display-week-plan (reverse results) week-num)))

(defun meal-planner--display-week-plan (week-plan week-num)
  "Display the generated week plan in a formatted buffer."
  (let ((buffer (get-buffer-create "*Meal Plan*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "=== MEAL PLAN - Week %d ===\n\n" week-num))
      
      (dolist (period-plan week-plan)
        (let ((period (car period-plan))
              (meals (cdr period-plan)))
          
          (if (eq period 'weekdays)
              (progn
                (insert "WEEKDAYS (Monday-Friday):\n"))
            (insert "WEEKEND (Saturday-Sunday):\n"))
          
          (dolist (meal-entry meals)
            (let* ((category (car meal-entry))
                   (meal (cdr meal-entry))
                   (category-name (meal-planner--format-category-name category)))
              (insert (format "  %s: %s\n" category-name meal))))
          
          (insert "\n")))
      
      ;; Add a detailed daily breakdown
      ;; (insert "=== DAILY BREAKDOWN ===\n\n")
      ;; (let ((weekday-meals (cdr (assq 'weekdays week-plan)))
      ;;       (weekend-meals (cdr (assq 'weekend week-plan))))
        
      ;;   (dolist (day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"))
      ;;     (insert (format "%s:\n" day))
      ;;     (dolist (meal-entry weekday-meals)
      ;;       (let* ((category (car meal-entry))
      ;;              (meal (cdr meal-entry))
      ;;              (category-name (meal-planner--format-category-name category)))
      ;;         (insert (format "  %s: %s\n" category-name meal))))
      ;;     (insert "\n"))
        
      ;;   (dolist (day '("Saturday" "Sunday"))
      ;;     (insert (format "%s:\n" day))
      ;;     (dolist (meal-entry weekend-meals)
      ;;       (let* ((category (car meal-entry))
      ;;              (meal (cdr meal-entry))
      ;;              (category-name (meal-planner--format-category-name category)))
      ;;         (insert (format "  %s: %s\n" category-name meal))))
      ;;     (insert "\n")))
      
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun meal-planner--format-category-name (category)
  "Format category symbol into readable name."
  (let ((name (symbol-name category)))
    (capitalize (replace-regexp-in-string "-" " " name))))

(defun meal-planner-setup-data-files ()
  "Create sample data files for meal categories."
  (interactive)
  (meal-planner--ensure-data-directory)
  
  (let ((sample-data 
         '((weekday-lunch . ("Sandwich and fruit" "Salad bowl" "Soup and bread" 
                           "Leftover curry" "Pasta salad" "Wrap and crisps"))
           (weekday-sweet . ("Apple slices" "Yogurt" "Biscuits" "Fruit bar" 
                           "Chocolate" "Dried fruit"))
           (weekday-dinner . ("Chicken curry" "Spaghetti bolognese" "Stir fry" 
                            "Fish and chips" "Roast dinner" "Pizza"))
           (weekday-pudding . ("Ice cream" "Fruit crumble" "Chocolate mousse" 
                             "Cheesecake" "Trifle" "Fresh fruit"))
           (weekend-lunch . ("Full English breakfast" "Pancakes" "Roast lunch" 
                           "BBQ" "Picnic spread" "Brunch"))
           (weekend-dinner . ("Slow cooked beef" "Homemade pizza" "Curry night" 
                            "Fish pie" "Lasagna" "Sunday roast"))
           (weekend-dessert . ("Homemade cake" "Fruit tart" "Ice cream sundae" 
                             "Chocolate brownies" "Apple pie" "Tiramisu")))))
    
    (dolist (category-data sample-data)
      (let* ((category (car category-data))
             (meals (cdr category-data))
             (file (meal-planner--get-data-file category)))
        (with-temp-file file
          (insert (mapconcat 'identity meals "\n")))
        (message "Created %s with %d meals" file (length meals))))))

(defun meal-planner-edit-category (category)
  "Edit meals for a specific category."
  (interactive 
   (list (intern (completing-read "Edit category: " 
                                 (mapcar (lambda (x) (symbol-name (car x))) 
                                        meal-planner-categories)
                                 nil t))))
  (let ((file (meal-planner--get-data-file category)))
    (meal-planner--ensure-data-directory)
    (unless (file-exists-p file)
      (with-temp-file file (insert "")))
    (find-file file)))

(defun meal-planner-view-history ()
  "View meal selection history."
  (interactive)
  (meal-planner--load-history)
  (let ((buffer (get-buffer-create "*Meal History*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== MEAL SELECTION HISTORY ===\n\n")
      (if meal-planner-history
          (dolist (week-entry (reverse meal-planner-history))
            (insert (format "Week %d:\n" (car week-entry)))
            (dolist (period-entry (cdr week-entry))
              (let ((period (car period-entry)))
                (insert (format "  %s:\n" 
                              (if (eq period 'weekdays) "WEEKDAYS" "WEEKEND")))
                (dolist (category-entry (cdr period-entry))
                  (insert (format "    %s: %s\n" 
                                (meal-planner--format-category-name (car category-entry))
                                (mapconcat 'identity (cdr category-entry) ", "))))))
            (insert "\n"))
        (insert "No meal history found.\n"))
      (goto-char (point-min)))
    (display-buffer buffer)))

;;;###autoload
(defun meal-planner-clear-history ()
  "Clear meal selection history."
  (interactive)
  (when (yes-or-no-p "Clear all meal history? ")
    (setq meal-planner-history nil)
    (meal-planner--save-history)
    (message "Meal history cleared.")))

(provide 'meal-planner)

;;; meal-planner.el ends here
