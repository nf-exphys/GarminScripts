import imaplib, email
user = 'nick4man@gmail.com'
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
con.login(user, password)

# calling function to check for email under this label 
con.select('Inbox')

msgs = get_emails(search('FROM', 'support@elitehrv.com', con))

newest_msg =  len(msgs)-1

msg_to_read = msgs[newest_msg]

msg_to_read = str(msg_to_read)

start = msg_to_read.find('/export/')

if(start == -1):
    print("bad find")

end = start + 100

msg_to_return = msg_to_read[start:end]

password = None

link = msg_to_return.split(sep = "</a>")

link = link[0]

link = str('https://app.elitehrv.com' + link)
