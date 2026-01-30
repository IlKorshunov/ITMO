import pytest
from unittest.mock import Mock, patch
from interview_coach.llm.langchain_provider import LangChainHuggingFaceProvider, Message

class TestLangChainProvider:
    @patch('interview_coach.llm.langchain_provider.HuggingFaceEndpoint')
    @patch('interview_coach.llm.langchain_provider.ChatHuggingFace')
    def test_generate_sync(self, mock_chat, mock_endpoint):
        mock_chat_instance = Mock()
        mock_chat_instance.invoke.return_value.content = "Hello"
        mock_chat.return_value = mock_chat_instance
        
        provider = LangChainHuggingFaceProvider(api_token="test", model="test")
        messages = [Message(role="user", content="Hi")]
        
        response = provider.generate_sync(messages)
        assert response == "Hello"
        
    @patch('interview_coach.llm.langchain_provider.HuggingFaceEndpoint')
    @patch('interview_coach.llm.langchain_provider.ChatHuggingFace')
    def test_json_instruction(self, mock_chat, mock_endpoint):
        mock_chat_instance = Mock()
        mock_chat_instance.invoke.return_value.content = '{"key": "value"}'
        mock_chat.return_value = mock_chat_instance
        
        provider = LangChainHuggingFaceProvider(api_token="test", model="test")
        messages = [Message(role="user", content="Hi")]
        
        provider.generate_sync(messages, json_mode=True)
        
        # Check if instruction was added
        args = mock_chat_instance.invoke.call_args[0][0]
        assert "JSON" in args[-1].content
