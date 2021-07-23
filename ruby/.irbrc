if ENV["INSIDE_EMACS"] then
   puts "Inside Emacs we are.  Simple prompt we need."

   IRB.conf[:USE_MULTILINE] = false
   IRB.conf[:USE_SINGLELINE] = false

   IRB.conf[:PROMPT_MODE] = :INF_RUBY
   IRB.conf[:USE_READLINE] = false
   IRB.conf[:USE_COLORIZE] = true
end

require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 200
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"
