var user = new Object();
$(function() {
	Order.init();
});

var Order = {
	'inputs': null,
	
	'init': function() {
		this.initTabs();
		this.initInputs();
		this.initUsers();
		this.initPwForm();
		this.initOrders();
	},
	
	'initTabs': function() {
		$('#Tabs').tabs();
	},
	
	'initInputs': function() {
		this.inputs = $('input[id^=Order-]');
		this.inputs.change(function() {
			if(!parseInt($(this).val())) {
				$(this).val(0);
			}
			$(this).val(Math.max(0, $(this).val()));
		});
	},
	
	'initUsers': function() {
		$('#Users').dialog({
			autoOpen: false,
			bgiframe: true,
			modal: true,
			width: 1000,
			height: 500,
			open: function() { $(this).find('.user').removeClass('selected'); }
		}).tabs();
	},
	
	'initPwForm': function() {
		$('#EnterPw').dialog({
			autoOpen: false,
			bgiframe: true,
			modal: true,
			width: 550,
			height: 290,
			open: function() { $(this).find('.input').val(''); }
		});
	},
	
	'initOrders': function() {
		var $this = this;
		$('#Orders').dialog({
			autoOpen: false,
			bgiframe: true,
			modal: true,
			width: 500,
			height: 500,
			open: function() { $(this).empty(); },
			close: function() { $this.resetUser(); },
			buttons: {
				Schließen: function() {
					$(this).dialog('close');
				}
			}
		});
	},
	
	'alert': function(title, message, options) {
		var options = $.extend({
			bgiframe: true,
			modal: true,
			buttons: {
				OK: function() {
					$(this).dialog('close');
				}
			}
		}, options);
		if(typeof options == 'object') {
			
		}
		$('<div></div>').attr('title', title).append(
			$('<span></span>').addClass('ui-icon').addClass('ui-icon-alert').css({'float': 'left', 'margin': '0 7px 20px 0'})
		).append(
			$('<span></span>').html(message)
		).dialog(options);
	},
	
	'countUp': function(articleId) {
		var $article = this.inputs.filter('#Order-' + articleId);
		$article.val(parseInt($article.val()) + 1).change();
	},
	
	'countDown': function(articleId) {
		var $article = this.inputs.filter('#Order-' + articleId);
		$article.val(parseInt($article.val()) - 1).change();
	},
	
	'resetOrder': function() {
		$('#Tabs input:text').val(0);
	},
	
	'showUserList': function() {
		if(!this.inputs.filter('[value!="0"]').length) {
			this.alert('Fehler', 'Bestellung ist leer.');
		} else {
			$('#Users').dialog('option', 'buttons', {
				Abbrechen: function() {
					$(this).dialog('close');
				}
			}).dialog('open');
			$('#Users input[name="do"]').attr('value', 'order');
		}
	},
	
	'selectUser': function(userId, username) {
		user.id = userId;
		var action = $('#Users input[name="do"]').val();
		if(action == 'order') {
			$('#EnterPw').dialog('option', 'buttons', {
				Abbrechen: function() {
					$(this).dialog('close');
				},
				OK: function() {
					user.pw = $(this).find('.input').val();
					$(this).dialog('close');
					
					Order.doOrder();
				}
			}).dialog('open');
		} else if(action == 'showOrders') {
			$('#EnterPw').dialog('option', 'buttons', {
				Abbrechen: function() {
					$(this).dialog('close');
				},
				OK: function() {
					user.pw = $(this).find('.input').val();
					$(this).dialog('close');
					
					Order.showOrdersForUser();
				}
			}).dialog('open');
		}
	},
	
	'addPwSign': function(sign) {
		$('#EnterPw .input').focus();
		var $pwInput = $("#EnterPw .input");
		$pwInput.val($pwInput.val() + sign + "");
	},
	
	'removePwSign': function() {
		$('#EnterPw .input').focus();
		var pw = $("#EnterPw .input").val();
		if(pw) {
			pw = pw.substr(0, pw.length-1);
			$("#EnterPw .input").val(pw);
		}
	},
	
	'keyboardToogle': function() {
		$('#keyboardlower').toggle();
		$('#keyboardupper').toggle();
	},
	
	'doOrder': function() {
		var $this = this;
		
		var articles = new Array();
		this.inputs.filter('[value!="0"]').each(function() {
			var articleId = $(this).attr('id').split('-');
			articleId = articleId[articleId.length - 1];
			article = new Object();
			article.id = articleId;
			article.amount = $(this).val()
			articles.push(article);
		});
		
		$.post('index.php', {
			'do': 'order',
			'user': JSON.encode(user),
			'articles': JSON.encode(articles)
		}, function(ret) {
			$this.resetUser();
			var options = {
				buttons: {
					OK: function() {
						$(this).dialog('close');
						if(ret.code == 0) {
							return;
						}
						$('.ui-dialog-content').each(function() {
							if($(this).dialog('isOpen')) {
								$(this).dialog('close');
							}
						});
					}
				}
			};
			$this.alert('Bestellung speichern', ret.message, options);
			if(ret.code != 0) {
				$this.resetOrder();
			}
		}, 'json');
	},
	
	'showOrders': function() {
		$('#Users input[name="do"]').attr('value', 'showOrders');
		$('#Users').dialog('option', 'buttons', {
			Abbrechen: function() {
				$(this).dialog('close');
			}
		}).dialog('open');
	},
	
	'showOrdersForUser': function() {
		var $this = this;
		
		$.post('index.php', {
			'do': 'showOrders',
			'user': JSON.encode(user)
		}, function(ret) {
			if(ret.code == 0) {
				$this.alert('Bestellungen einsehen', ret.message);
			} else {
				$('#Orders').dialog('option', 'title', 'Bestellungen von ' + ret.username).dialog('open').html(ret.message);
			}
		}, 'json');
	},
	
	'resetUser': function() {
		user = new Object();
	}
};
