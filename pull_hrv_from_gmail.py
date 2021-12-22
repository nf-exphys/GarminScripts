import imaplib, email

user = 'hrv4nick@gmail.com'
password = input("Enter gmail pwd:")
imap_url = 'imap.gmail.com'
 # Function to get email content part i.e its body part 
def get_body(msg): 
    if msg.is_multipart(): 
        return get_body(msg.get_payload(0)) 
    else: 
        return msg.get_payload(None, True)

 # Function to search for a key value pair  
def search(key, value, con):  
    result, data = con.search(None, key, '"{}"'.format(value)) 
    return data

 # Function to get the list of emails under this label 
def get_emails(result_bytes): 
    msgs = [] # all the email data are pushed inside an array 
    for num in result_bytes[0].split(): 
        typ, data = con.fetch(num, '(RFC822)') 
        msgs.append(data) 
  
    return msgs

 # this is done to make SSL connnection with GMAIL 
con = imaplib.IMAP4_SSL(imap_url)

 # logging the user in
 # sometimes throws error when Google blocks the login
con.login(user, password)

# calling function to check for email under this label 
con.select('Inbox')

msgs = get_emails(search('FROM', 'support@elitehrv.com', con))

newest_msg =  len(msgs)-1


msg_to_read = msgs[newest_msg][0]

msg_to_read = msg_to_read[1]

msg_to_read = msg_to_read.decode("utf-8")

# msg_to_read = str(msg_to_read)

# links = re.findall("http.*</a>", msg_to_read)
# 
# links = links[0]
# 
# links = links.split(sep = "http")
# 
# links[2]

start = re.search('https', msg_to_read).start()

end = msg_to_read.find('</a>')

# start = msg_to_read.find('https:')



#this means it didn't find the location of the link in the email
if(start == -1):
    print("bad find")

msg_to_return = msg_to_read[start:end]

msg_to_return

password = None

second_half = msg_to_return.split(sep = "Please click the following link to access your export:")

def listToString(s): 
    
    # initialize an empty string
    str1 = " " 
    
    #make string
    str1 = str1.join(s)
    
    #remove annoying spaces from string
    str2 = re.sub(pattern=" ", repl="", string=str1)
    
    return (str2)


second_half = second_half[1]

second_half = listToString(second_half)

type(second_half)

no_rn = re.sub(pattern="\r\n", repl="", string=second_half)

start = re.search('https', no_rn).start()

end = len(no_rn)

link = no_rn[start:end]

link = re.sub(pattern="=", repl="", string=link)
